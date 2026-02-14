(ns lsp-mcp.core-test
  "Integration tests for lsp-mcp.core — the public API orchestration layer.

   Tests the full pipeline: analyze → extract → transform → sync to KG.
   All external dependencies (analysis, bridge) are mocked via with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [lsp-mcp.core :as core]))

;; =============================================================================
;; Test Data
;; =============================================================================

(def sample-raw-analysis
  "Minimal analysis result as returned by analysis/analyze-project!."
  {:analysis {"file://src/my/app/core.clj"
              {:var-definitions [{:ns 'my.app.core :name 'start!
                                  :row 10 :col 1
                                  :arglist-strs ["[config]"]
                                  :private false :macro false
                                  :defined-by 'clojure.core/defn}
                                 {:ns 'my.app.core :name 'stop!
                                  :row 25 :col 1
                                  :arglist-strs ["[]"]
                                  :private false :macro false
                                  :defined-by 'clojure.core/defn}
                                 {:ns 'my.app.core :name 'helper
                                  :row 40 :col 1
                                  :arglist-strs ["[x]"]
                                  :private true :macro false
                                  :defined-by 'clojure.core/defn}]
               :var-usages [{:from 'my.app.core :from-var 'start!
                             :to 'my.app.db :name 'connect
                             :row 12}
                            {:from 'my.app.core :from-var 'start!
                             :to 'my.app.db :name 'query
                             :row 14}]}
              "file://src/my/app/db.clj"
              {:var-definitions [{:ns 'my.app.db :name 'connect
                                  :row 5 :col 1
                                  :arglist-strs nil
                                  :private false :macro false
                                  :defined-by 'clojure.core/def}
                                 {:ns 'my.app.db :name 'query
                                  :row 15 :col 1
                                  :arglist-strs ["[sql params]"]
                                  :private false :macro false
                                  :defined-by 'clojure.core/defn}]
               :var-usages []}}
   :dep-graph {'my.app.core {:dependencies {'my.app.db 1}
                             :dependents   {}
                             :internal?    true}
               'my.app.db   {:dependencies {}
                             :dependents   {'my.app.core 1}
                             :internal?    true}}})

;; =============================================================================
;; analyze tests
;; =============================================================================

(deftest analyze-test
  (testing "analyze delegates to analysis/analyze-project! and returns raw result"
    (with-redefs [lsp-mcp.analysis/analyze-project! (constantly sample-raw-analysis)]
      (let [result (core/analyze "/test-project")]
        (is (map? result))
        (is (contains? result :analysis))
        (is (contains? result :dep-graph))
        (is (= 2 (count (:analysis result))))
        (is (= 2 (count (:dep-graph result)))))))

  (testing "analyze propagates error map from analysis"
    (with-redefs [lsp-mcp.analysis/analyze-project!
                  (constantly {:error "No analysis available for test"})]
      (let [result (core/analyze "/test-project")]
        (is (contains? result :error))))))

;; =============================================================================
;; analyze-and-sync! tests
;; =============================================================================

(deftest analyze-and-sync!-full-pipeline-test
  (testing "full pipeline: analyze → extract → transform → sync"
    (let [synced-entries (atom [])
          synced-edges   (atom [])
          mock-sync      (fn [_project-id operations _scope]
                           (reset! synced-entries (:memory-entries operations))
                           (reset! synced-edges (:kg-edges operations))
                           {:created (count (:memory-entries operations))
                            :edges   (count (:kg-edges operations))
                            :errors  []})]
      (with-redefs [lsp-mcp.analysis/analyze-project! (constantly sample-raw-analysis)
                    lsp-mcp.kg-bridge/sync-to-kg!     mock-sync]
        (let [result (core/analyze-and-sync! "/test-project" "test-proj" "project")]
          ;; Top-level structure
          (is (map? result))
          (is (contains? result :analysis-stats))
          (is (contains? result :sync-stats))

          ;; Analysis stats
          (let [stats (:analysis-stats result)]
            (is (number? (:time-ms stats)))
            ;; 5 total var-defs (start!, stop!, helper, connect, query — includes private)
            (is (= 5 (:var-defs stats)))
            ;; 2 call edges (start!→connect, start!→query)
            (is (= 2 (:calls stats)))
            ;; 2 namespaces
            (is (= 2 (:nses stats))))

          ;; Sync stats
          (let [sync (:sync-stats result)]
            (is (number? (:time-ms sync)))
            (is (map? (:result sync)))
            (is (= 0 (count (:errors (:result sync))))))

          ;; Verify operations were passed to sync
          ;; Memory entries: 4 public var entries + 2 namespace entries = 6
          (is (= 6 (count @synced-entries)))
          (is (every? #(= "snippet" (:type %)) @synced-entries))

          ;; KG edges: 2 call edges + 1 ns-dep edge (core→db) = 3
          (is (pos? (count @synced-edges)))
          (is (every? :from-key @synced-edges))
          (is (every? :to-key @synced-edges)))))))

(deftest analyze-and-sync!-empty-project-test
  (testing "empty project produces zero stats"
    (with-redefs [lsp-mcp.analysis/analyze-project! (constantly {:analysis {} :dep-graph {}})
                  lsp-mcp.kg-bridge/sync-to-kg!     (fn [_ _ _] {:created 0 :edges 0 :errors []})]
      (let [result (core/analyze-and-sync! "/empty" "empty-proj" "project")
            stats  (:analysis-stats result)]
        (is (= 0 (:var-defs stats)))
        (is (= 0 (:calls stats)))
        (is (= 0 (:nses stats)))))))

(deftest analyze-and-sync!-graceful-degradation-test
  (testing "sync failure returns error info without crashing"
    (with-redefs [lsp-mcp.analysis/analyze-project! (constantly sample-raw-analysis)
                  lsp-mcp.kg-bridge/sync-to-kg!     (fn [_ _ _]
                                                      {:created 0
                                                       :edges   0
                                                       :errors  ["Bridge not available"]})]
      (let [result (core/analyze-and-sync! "/test" "test" "project")]
        ;; Analysis still succeeds (5 total var-defs including private)
        (is (= 5 (get-in result [:analysis-stats :var-defs])))
        ;; Sync reports errors
        (is (= ["Bridge not available"]
               (get-in result [:sync-stats :result :errors])))))))

;; =============================================================================
;; status tests
;; =============================================================================

(deftest status-test
  (testing "status returns bridge and cache info"
    (with-redefs [lsp-mcp.kg-bridge/available?  (constantly false)
                  lsp-mcp.cache/cache-status     (constantly {:cache-dir "/tmp/test" :projects []})]
      (let [result (core/status)]
        (is (false? (:bridge-available? result)))
        (is (= "/tmp/test" (get-in result [:cache :cache-dir]))))))

  (testing "status with available bridge"
    (with-redefs [lsp-mcp.kg-bridge/available?  (constantly true)
                  lsp-mcp.cache/cache-status     (constantly {:cache-dir "/tmp/test" :projects []})]
      (let [result (core/status)]
        (is (true? (:bridge-available? result)))))))

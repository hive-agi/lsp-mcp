(ns lsp-mcp.core-test
  "Integration tests for lsp-mcp.core — the public API orchestration layer.

   Tests the full railway-oriented pipeline:
     analyze → extract (parallel) → transform → sync to KG.
   All external dependencies (analysis, bridge) are mocked via with-redefs.
   Mocks return Results; assertions unwrap with r/ok?/r/err?."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [lsp-mcp.core :as core]))

;; =============================================================================
;; Test Data
;; =============================================================================

(def sample-raw-analysis
  "Minimal analysis result as returned by analysis/analyze-project! (unwrapped)."
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
  (testing "analyze delegates to analysis/analyze-project! and returns ok Result"
    (with-redefs [lsp-mcp.analysis/analyze-project! (constantly (r/ok sample-raw-analysis))]
      (let [result (core/analyze "/test-project")]
        (is (r/ok? result))
        (let [raw (:ok result)]
          (is (contains? raw :analysis))
          (is (contains? raw :dep-graph))
          (is (= 2 (count (:analysis raw))))
          (is (= 2 (count (:dep-graph raw))))))))

  (testing "analyze propagates err Result from analysis"
    (with-redefs [lsp-mcp.analysis/analyze-project!
                  (constantly (r/err :analysis/lsp-unavailable {:message "test"}))]
      (let [result (core/analyze "/test-project")]
        (is (r/err? result))
        (is (= :analysis/lsp-unavailable (:error result)))))))

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
                           (r/ok {:created (count (:memory-entries operations))
                                  :edges   (count (:kg-edges operations))
                                  :errors  []}))]
      (with-redefs [lsp-mcp.analysis/analyze-project! (constantly (r/ok sample-raw-analysis))
                    lsp-mcp.kg-bridge/sync-to-kg!     mock-sync]
        (let [result (core/analyze-and-sync! "/test-project" "test-proj" "project")]
          (is (r/ok? result))
          (let [v (:ok result)]
            (is (contains? v :analysis-stats))
            (is (contains? v :sync-stats))

            (let [stats (:analysis-stats v)]
              (is (number? (:time-ms stats)))
              (is (= 5 (:var-defs stats)))
              (is (= 2 (:calls stats)))
              (is (= 2 (:nses stats))))

            (let [sync (:sync-stats v)]
              (is (number? (:time-ms sync)))
              (is (map? (:result sync)))
              (is (= 0 (count (:errors (:result sync)))))))

          ;; 4 public var entries + 2 namespace entries = 6
          (is (= 6 (count @synced-entries)))
          (is (every? #(= "snippet" (:type %)) @synced-entries))

          ;; KG edges: 2 call edges + 1 ns-dep edge = 3
          (is (pos? (count @synced-edges)))
          (is (every? :from-key @synced-edges))
          (is (every? :to-key @synced-edges)))))))

(deftest analyze-and-sync!-empty-project-test
  (testing "empty project produces zero stats"
    (with-redefs [lsp-mcp.analysis/analyze-project!
                  (constantly (r/ok {:analysis {} :dep-graph {}}))
                  lsp-mcp.kg-bridge/sync-to-kg!
                  (fn [_ _ _] (r/ok {:created 0 :edges 0 :errors []}))]
      (let [result (core/analyze-and-sync! "/empty" "empty-proj" "project")]
        (is (r/ok? result))
        (let [stats (get-in result [:ok :analysis-stats])]
          (is (= 0 (:var-defs stats)))
          (is (= 0 (:calls stats)))
          (is (= 0 (:nses stats))))))))

(deftest analyze-and-sync!-graceful-degradation-test
  (testing "sync failure surfaced via Result :errors"
    (with-redefs [lsp-mcp.analysis/analyze-project! (constantly (r/ok sample-raw-analysis))
                  lsp-mcp.kg-bridge/sync-to-kg!     (fn [_ _ _]
                                                      (r/ok {:created 0
                                                             :edges   0
                                                             :errors  ["Bridge not available"]}))]
      (let [result (core/analyze-and-sync! "/test" "test" "project")]
        (is (r/ok? result))
        ;; Analysis still succeeds (5 total var-defs including private)
        (is (= 5 (get-in result [:ok :analysis-stats :var-defs])))
        ;; Sync errors propagated
        (is (= ["Bridge not available"]
               (get-in result [:ok :sync-stats :result :errors])))))))

(deftest analyze-and-sync!-short-circuits-on-analysis-err
  (testing "analyze err short-circuits the pipeline; sync never invoked"
    (let [sync-called? (atom false)]
      (with-redefs [lsp-mcp.analysis/analyze-project!
                    (constantly (r/err :analysis/missing-root {:message "boom"}))
                    lsp-mcp.kg-bridge/sync-to-kg!
                    (fn [_ _ _] (reset! sync-called? true) (r/ok {}))]
        (let [result (core/analyze-and-sync! "" "test" "project")]
          (is (r/err? result))
          (is (= :analysis/missing-root (:error result)))
          (is (false? @sync-called?)))))))

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

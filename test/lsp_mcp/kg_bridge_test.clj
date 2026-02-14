(ns lsp-mcp.kg-bridge-test
  "Tests for lsp-mcp.kg-bridge namespace."
  (:require [clojure.test :as t :refer [deftest]]
            [lsp-mcp.kg-bridge :as kg]))

;; =============================================================================
;; available?
;; =============================================================================

(deftest available?-test
  (t/testing "available? returns falsy when hive-mcp not on classpath"
    (t/is (not (kg/available?)))))

;; =============================================================================
;; sync-to-kg! — graceful degradation
;; =============================================================================

(deftest sync-to-kg!-graceful-degradation
  (t/testing "sync-to-kg! returns zero counts when requiring-resolve fails"
    (let [ops {:memory-entries [{:type "snippet" :content "(defn foo [x])\n  Location: src/my.clj:10"
                                 :tags ["lsp" "function-def" "ns:my.ns"]
                                 :duration "medium"
                                 :key "ns:my.ns/foo"}]
               :kg-edges [{:from-key "ns:my.ns/foo" :to-key "ns:other.ns/bar"
                           :relation :depends-on :confidence 1.0
                           :source-type :automated :created-by "lsp-mcp"}]
               :stats {:fns 1 :edges 1 :namespaces 0}}
          result (kg/sync-to-kg! "test-project" ops "test-project")]
      ;; With no hive-mcp on classpath, resolve-fn fails -> no entries created
      (t/is (= 0 (:created result)))
      (t/is (= 0 (:edges result)))
      ;; Entry failures are tracked when index-fn is nil
      (t/is (vector? (:errors result))))))

;; =============================================================================
;; sync-to-kg! — mocked hive-mcp integration
;; =============================================================================

(deftest sync-to-kg!-with-mock
  (t/testing "sync-to-kg! processes memory entries and edges via mocked fns"
    (let [added-memory  (atom [])
          added-edges   (atom [])
          entry-counter (atom 0)
          mock-index  (fn [entry-map]
                        (swap! added-memory conj entry-map)
                        (str "id-" (swap! entry-counter inc)))
          mock-edge   (fn [edge-map]
                        (swap! added-edges conj edge-map)
                        (str "edge-" (count @added-edges)))
          mock-hash   (fn [content] (str "hash-" (hash content)))
          mock-scope  (fn [tags project-id]
                        (conj (vec tags) (str "scope:project:" project-id)))]
      (with-redefs [lsp-mcp.kg-bridge/resolve-fn
                    (fn [sym]
                      (case (str sym)
                        "hive-mcp.chroma/index-memory-entry!" mock-index
                        "hive-mcp.knowledge-graph.edges/add-edge!" mock-edge
                        "hive-mcp.chroma/content-hash" mock-hash
                        "hive-mcp.chroma/find-duplicate" (constantly nil)
                        "hive-mcp.tools.memory.scope/inject-project-scope" mock-scope
                        nil))]
        (let [ops {:memory-entries [{:type "snippet" :content "(defn bar [])"
                                     :tags ["lsp"] :duration "medium"
                                     :key "ns:test.ns/bar"}
                                    {:type "snippet" :content "(defn baz [x])"
                                     :tags ["lsp"] :duration "medium"
                                     :key "ns:other.ns/baz"}]
                   :kg-edges [{:from-key "ns:test.ns/bar" :to-key "ns:other.ns/baz"
                               :relation :depends-on :confidence 0.9
                               :source-type :automated :created-by "lsp-mcp"}]
                   :stats {}}
              result (kg/sync-to-kg! "test" ops "test")]
          (t/is (= 2 (:created result)))
          (t/is (= 1 (:edges result)))
          (t/is (empty? (:errors result)) "errors should be empty on success")
          (t/is (= 2 (count @added-memory)))
          (t/is (= 1 (count @added-edges)))
          ;; Verify scope injection happened
          (t/is (some #(= "scope:project:test" %)
                      (:tags (first @added-memory)))
                "scope tag should be injected")
          ;; Verify content-hash was attached
          (t/is (contains? (first @added-memory) :content-hash)
                "content-hash should be set")
          ;; Verify project-id was passed through
          (t/is (= "test" (:project-id (first @added-memory)))
                "project-id should be set on entry"))))))

;; =============================================================================
;; sync-to-kg! — dedup
;; =============================================================================

(deftest sync-to-kg!-dedup-test
  (t/testing "sync-to-kg! reuses existing entry IDs on duplicate"
    (let [added-memory (atom [])
          mock-dup    (fn [_type _hash & _kv]
                        {:id "existing-123"})
          mock-index  (fn [_] (swap! added-memory conj _) "should-not-be-called")
          mock-hash   (fn [content] (str "hash-" (hash content)))]
      (with-redefs [lsp-mcp.kg-bridge/resolve-fn
                    (fn [sym]
                      (case (str sym)
                        "hive-mcp.chroma/index-memory-entry!" mock-index
                        "hive-mcp.knowledge-graph.edges/add-edge!" (constantly "e1")
                        "hive-mcp.chroma/content-hash" mock-hash
                        "hive-mcp.chroma/find-duplicate" mock-dup
                        "hive-mcp.tools.memory.scope/inject-project-scope" nil
                        nil))]
        (let [ops {:memory-entries [{:type "snippet" :content "(defn foo [])"
                                     :tags ["lsp"] :duration "medium"
                                     :key "ns:x/foo"}]
                   :kg-edges []
                   :stats {}}
              result (kg/sync-to-kg! "proj" ops "proj")]
          ;; Entry reused from duplicate, not indexed again
          (t/is (= 1 (:created result)))
          (t/is (empty? @added-memory) "index-memory-entry! should not be called for dups"))))))

;; =============================================================================
;; sync-to-kg! — unresolved edges
;; =============================================================================

(deftest sync-to-kg!-unresolved-edges-test
  (t/testing "edges with unresolvable nodes are skipped (not errors)"
    (let [mock-index (fn [_] "id-1")]
      (with-redefs [lsp-mcp.kg-bridge/resolve-fn
                    (fn [sym]
                      (case (str sym)
                        "hive-mcp.chroma/index-memory-entry!" mock-index
                        "hive-mcp.knowledge-graph.edges/add-edge!" (constantly "e1")
                        nil))]
        (let [ops {:memory-entries [{:type "snippet" :content "x"
                                     :tags [] :duration "medium"
                                     :key "a/foo"}]
                   ;; Edge references "b/bar" which has no memory entry
                   :kg-edges [{:from-key "a/foo" :to-key "b/bar"
                               :relation :depends-on}]
                   :stats {}}
              result (kg/sync-to-kg! "p" ops "p")]
          (t/is (= 1 (:created result)))
          (t/is (= 0 (:edges result)) "edge should be skipped, not created")
          (t/is (empty? (:errors result)) "unresolved edges are debug, not errors"))))))

;; =============================================================================
;; sync-to-kg! — empty operations
;; =============================================================================

(deftest sync-to-kg!-empty-operations
  (t/testing "handles empty operations gracefully"
    (let [ops {:memory-entries [] :kg-edges [] :stats {}}
          result (kg/sync-to-kg! "test" ops "test")]
      (t/is (= 0 (:created result)))
      (t/is (= 0 (:edges result)))
      (t/is (empty? (:errors result))))))

;; =============================================================================
;; sync-to-kg! — edge failure tracking
;; =============================================================================

(deftest sync-to-kg!-edge-failure-tracked
  (t/testing "failed edge additions are tracked in :errors"
    (let [mock-index (fn [_] (str "id-" (rand-int 1000)))]
      (with-redefs [lsp-mcp.kg-bridge/resolve-fn
                    (fn [sym]
                      (case (str sym)
                        "hive-mcp.chroma/index-memory-entry!" mock-index
                        ;; add-edge! returns nil (failure)
                        "hive-mcp.knowledge-graph.edges/add-edge!" (constantly nil)
                        nil))]
        (let [ops {:memory-entries [{:type "snippet" :content "a" :tags ["lsp"] :duration "medium"
                                     :key "ns:x/a"}
                                    {:type "snippet" :content "b" :tags ["lsp"] :duration "medium"
                                     :key "ns:y/b"}]
                   :kg-edges [{:from-key "ns:x/a" :to-key "ns:y/b"
                               :relation :depends-on :confidence 1.0
                               :source-type :automated :created-by "lsp-mcp"}]
                   :stats {}}
              result (kg/sync-to-kg! "test" ops "test")]
          (t/is (= 2 (:created result)))
          (t/is (= 0 (:edges result)) "edge count 0 when add-kg-edge! returns nil")
          (t/is (= 1 (count (:errors result))) "one error for the failed edge")
          (t/is (.contains ^String (first (:errors result)) "Failed edge")
                "error message should mention failed edge"))))))

;; =============================================================================
;; sync-to-kg! — resolved IDs passed to edge API
;; =============================================================================

(deftest sync-to-kg!-resolved-ids-in-edges
  (t/testing "edges receive resolved memory IDs, not raw keys"
    (let [edge-args     (atom nil)
          entry-counter (atom 0)
          mock-index  (fn [_]
                        (str "mem-" (swap! entry-counter inc)))
          mock-edge   (fn [e]
                        (reset! edge-args e)
                        "edge-ok")]
      (with-redefs [lsp-mcp.kg-bridge/resolve-fn
                    (fn [sym]
                      (case (str sym)
                        "hive-mcp.chroma/index-memory-entry!" mock-index
                        "hive-mcp.knowledge-graph.edges/add-edge!" mock-edge
                        nil))]
        (let [ops {:memory-entries [{:type "snippet" :content "a" :tags ["lsp"] :duration "medium"
                                     :key "ns:x/a"}
                                    {:type "snippet" :content "b" :tags ["lsp"] :duration "medium"
                                     :key "ns:y/b"}]
                   :kg-edges [{:from-key "ns:x/a" :to-key "ns:y/b"
                               :relation :depends-on :confidence 1.0
                               :source-type :automated :created-by "lsp-mcp"}]
                   :stats {}}]
          (kg/sync-to-kg! "test" ops "test")
          (t/is (some? @edge-args) "mock-edge should have been called")
          (let [e @edge-args]
            ;; add-kg-edge! extracts :from and :to from the resolved edge-map
            (t/is (= "mem-1" (:from e)) ":from should be resolved memory ID")
            (t/is (= "mem-2" (:to e)) ":to should be resolved memory ID")))))))

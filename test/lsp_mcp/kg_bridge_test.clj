(ns lsp-mcp.kg-bridge-test
  "Tests for lsp-mcp.kg-bridge — Result-returning bridge + bounded-pmap writes."
  (:require [clojure.test :as t :refer [deftest]]
            [hive-dsl.result :as r]
            [lsp-mcp.kg-bridge :as kg]))

;; =============================================================================
;; available?
;; =============================================================================

(deftest available?-test
  (t/testing "available? returns falsy when hive-mcp not on classpath"
    (t/is (not (kg/available?)))))

;; =============================================================================
;; sync-to-kg! — graceful degradation (no hive-mcp on classpath)
;; =============================================================================

(deftest sync-to-kg!-graceful-degradation
  (t/testing "sync-to-kg! returns ok Result with errors when bridge missing"
    (let [ops {:memory-entries [{:type "snippet" :content "(defn foo [x])\n  Location: src/my.clj:10"
                                 :tags ["lsp" "function-def" "ns:my.ns"]
                                 :duration "medium"
                                 :key "ns:my.ns/foo"}]
               :kg-edges [{:from-key "ns:my.ns/foo" :to-key "ns:other.ns/bar"
                           :relation :depends-on :confidence 1.0
                           :source-type :automated :created-by "lsp-mcp"}]
               :stats {:fns 1 :edges 1 :namespaces 0}}
          result (kg/sync-to-kg! "test-project" ops "test-project")]
      (t/is (r/ok? result))
      (let [v (:ok result)]
        (t/is (= 0 (:created v)))
        (t/is (= 0 (:edges v)))
        (t/is (vector? (:errors v)))
        ;; entry index failed -> error tracked
        (t/is (pos? (count (:errors v))))))))

;; =============================================================================
;; sync-to-kg! — mocked hive-mcp facade
;; =============================================================================

(defn- mock-resolver
  "Build a resolver fn that maps facade syms to mock impls."
  [{:keys [index edge hash dup scope]}]
  (fn [sym]
    (case (str sym)
      "hive-mcp.vectordb.facade/index-memory-entry!"     index
      "hive-mcp.knowledge-graph.edges/add-edge!"         edge
      "hive-mcp.vectordb.facade/content-hash"            hash
      "hive-mcp.vectordb.facade/find-duplicate"          dup
      "hive-mcp.tools.memory.scope/inject-project-scope" scope
      nil)))

(deftest sync-to-kg!-with-mock
  (t/testing "sync-to-kg! processes memory entries and edges via mocked fns"
    (let [added-memory  (atom [])
          added-edges   (atom [])
          entry-counter (atom 0)
          mock-index    (fn [entry-map]
                          (swap! added-memory conj entry-map)
                          (str "id-" (swap! entry-counter inc)))
          mock-edge     (fn [edge-map]
                          (swap! added-edges conj edge-map)
                          (str "edge-" (count @added-edges)))
          mock-hash     (fn [content] (str "hash-" (hash content)))
          mock-scope    (fn [tags project-id]
                          (conj (vec tags) (str "scope:project:" project-id)))]
      (with-redefs [lsp-mcp.kg-bridge/resolve-fn
                    (mock-resolver {:index mock-index
                                    :edge  mock-edge
                                    :hash  mock-hash
                                    :dup   (constantly nil)
                                    :scope mock-scope})]
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
          (t/is (r/ok? result))
          (let [v (:ok result)]
            (t/is (= 2 (:created v)))
            (t/is (= 1 (:edges v)))
            (t/is (empty? (:errors v)) "errors should be empty on success"))
          (t/is (= 2 (count @added-memory)))
          (t/is (= 1 (count @added-edges)))
          (t/is (some #(= "scope:project:test" %)
                      (:tags (first @added-memory)))
                "scope tag should be injected")
          (t/is (contains? (first @added-memory) :content-hash)
                "content-hash should be set")
          (t/is (= "test" (:project-id (first @added-memory)))
                "project-id should be set on entry"))))))

;; =============================================================================
;; sync-to-kg! — dedup
;; =============================================================================

(deftest sync-to-kg!-dedup-test
  (t/testing "sync-to-kg! reuses existing entry IDs on duplicate"
    (let [indexed-once (atom 0)
          mock-index   (fn [_] (swap! indexed-once inc) "should-not-be-called")
          mock-hash    (fn [content] (str "hash-" (hash content)))
          mock-dup     (fn [_type _hash & _kv] {:id "existing-123"})]
      (with-redefs [lsp-mcp.kg-bridge/resolve-fn
                    (mock-resolver {:index mock-index
                                    :edge  (constantly "e1")
                                    :hash  mock-hash
                                    :dup   mock-dup
                                    :scope nil})]
        (let [ops {:memory-entries [{:type "snippet" :content "(defn foo [])"
                                     :tags ["lsp"] :duration "medium"
                                     :key "ns:x/foo"}]
                   :kg-edges []
                   :stats {}}
              result (kg/sync-to-kg! "proj" ops "proj")]
          (t/is (r/ok? result))
          (t/is (= 1 (:created (:ok result))))
          (t/is (zero? @indexed-once)
                "index-memory-entry! should not be called for dups"))))))

;; =============================================================================
;; sync-to-kg! — unresolved edges (skipped silently)
;; =============================================================================

(deftest sync-to-kg!-unresolved-edges-test
  (t/testing "edges with unresolvable nodes are skipped (not errors)"
    (let [mock-index (fn [_] "id-1")]
      (with-redefs [lsp-mcp.kg-bridge/resolve-fn
                    (mock-resolver {:index mock-index
                                    :edge  (constantly "e1")
                                    :hash  nil
                                    :dup   nil
                                    :scope nil})]
        (let [ops {:memory-entries [{:type "snippet" :content "x"
                                     :tags [] :duration "medium"
                                     :key "a/foo"}]
                   :kg-edges [{:from-key "a/foo" :to-key "b/bar"
                               :relation :depends-on}]
                   :stats {}}
              result (kg/sync-to-kg! "p" ops "p")]
          (t/is (r/ok? result))
          (let [v (:ok result)]
            (t/is (= 1 (:created v)))
            (t/is (= 0 (:edges v)) "edge skipped, not created")
            (t/is (empty? (:errors v)) "unresolved edges are debug, not errors")))))))

;; =============================================================================
;; sync-to-kg! — empty operations
;; =============================================================================

(deftest sync-to-kg!-empty-operations
  (t/testing "handles empty operations gracefully"
    (let [ops {:memory-entries [] :kg-edges [] :stats {}}
          result (kg/sync-to-kg! "test" ops "test")]
      (t/is (r/ok? result))
      (let [v (:ok result)]
        (t/is (= 0 (:created v)))
        (t/is (= 0 (:edges v)))
        (t/is (empty? (:errors v)))))))

;; =============================================================================
;; sync-to-kg! — edge failure tracking
;; =============================================================================

(deftest sync-to-kg!-edge-failure-tracked
  (t/testing "failed edge additions are tracked in :errors"
    (let [counter    (atom 0)
          mock-index (fn [_] (str "id-" (swap! counter inc)))
          ;; add-edge! throws -> wrapped as bridge/edge-failed err
          mock-edge  (fn [_] (throw (ex-info "boom" {})))]
      (with-redefs [lsp-mcp.kg-bridge/resolve-fn
                    (mock-resolver {:index mock-index
                                    :edge  mock-edge
                                    :hash  nil :dup nil :scope nil})]
        (let [ops {:memory-entries [{:type "snippet" :content "a" :tags ["lsp"] :duration "medium"
                                     :key "ns:x/a"}
                                    {:type "snippet" :content "b" :tags ["lsp"] :duration "medium"
                                     :key "ns:y/b"}]
                   :kg-edges [{:from-key "ns:x/a" :to-key "ns:y/b"
                               :relation :depends-on :confidence 1.0
                               :source-type :automated :created-by "lsp-mcp"}]
                   :stats {}}
              result (kg/sync-to-kg! "test" ops "test")]
          (t/is (r/ok? result))
          (let [v (:ok result)]
            (t/is (= 2 (:created v)))
            (t/is (= 0 (:edges v)) "edge count 0 when add-edge! throws")
            (t/is (= 1 (count (:errors v))) "one error for the failed edge")
            (t/is (.contains ^String (first (:errors v)) "Failed edge")
                  "error message should mention failed edge")))))))

;; =============================================================================
;; sync-to-kg! — resolved IDs passed to edge API
;; =============================================================================

(deftest sync-to-kg!-resolved-ids-in-edges
  (t/testing "edges receive resolved memory IDs, not raw keys"
    (let [edge-args     (atom nil)
          entry-counter (atom 0)
          mock-index    (fn [_] (str "mem-" (swap! entry-counter inc)))
          mock-edge     (fn [e] (reset! edge-args e) "edge-ok")]
      (with-redefs [lsp-mcp.kg-bridge/resolve-fn
                    (mock-resolver {:index mock-index
                                    :edge  mock-edge
                                    :hash  nil :dup nil :scope nil})]
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
            (t/is (#{"mem-1" "mem-2"} (:from e))
                  ":from should be a resolved memory id (parallel order non-deterministic)")
            (t/is (#{"mem-1" "mem-2"} (:to e))
                  ":to should be a resolved memory id")
            (t/is (not= (:from e) (:to e)))))))))

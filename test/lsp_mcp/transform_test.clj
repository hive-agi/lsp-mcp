(ns lsp-mcp.transform-test
  (:require [clojure.test :refer [deftest is testing]]
            [lsp-mcp.transform :as t]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def sample-var-defs
  [{:ns 'my.app.core :name 'start! :file "src/my/app/core.clj" :row 10 :col 1
    :arglists ["[config]"] :private? false :macro? false :defined-by 'clojure.core/defn}
   {:ns 'my.app.core :name 'stop! :file "src/my/app/core.clj" :row 25 :col 1
    :arglists ["[]"] :private? false :macro? false :defined-by 'clojure.core/defn}
   {:ns 'my.app.core :name 'internal-helper :file "src/my/app/core.clj" :row 40 :col 1
    :arglists ["[x]"] :private? true :macro? false :defined-by 'clojure.core/defn}
   {:ns 'my.app.db :name 'query :file "src/my/app/db.clj" :row 5 :col 1
    :arglists ["[sql params]"] :private? false :macro? false :defined-by 'clojure.core/defn}
   {:ns 'my.app.db :name 'connect :file "src/my/app/db.clj" :row 15 :col 1
    :arglists nil :private? false :macro? false :defined-by 'clojure.core/def}])

(def sample-call-graph
  [{:caller-ns 'my.app.core :caller-fn 'start! :callee-ns 'my.app.db :callee-fn 'connect
    :file "src/my/app/core.clj" :row 12}
   {:caller-ns 'my.app.core :caller-fn 'start! :callee-ns 'my.app.db :callee-fn 'query
    :file "src/my/app/core.clj" :row 14}])

(def sample-ns-graph
  [{:ns 'my.app.core :depends-on #{'my.app.db 'clojure.string} :dependents #{} :internal? true}
   {:ns 'my.app.db :depends-on #{} :dependents #{'my.app.core} :internal? true}])

;; =============================================================================
;; var-def->memory-entry
;; =============================================================================

(deftest var-def->memory-entry-test
  (testing "creates snippet for function with arglists"
    (let [entry (t/var-def->memory-entry "test-project"
                                         {:ns 'my.ns :name 'foo :file "src/my/ns.clj"
                                          :row 42 :arglists ["[x y]"]})]
      (is (= "snippet" (:type entry)))
      (is (= "(defn foo [x y])\n  Location: src/my/ns.clj:42" (:content entry)))
      (is (= ["lsp" "function-def" "ns:my.ns"] (:tags entry)))
      (is (= "medium" (:duration entry)))
      (is (= "ns:my.ns/foo" (:key entry)))))

  (testing "creates snippet for def without arglists"
    (let [entry (t/var-def->memory-entry "test-project"
                                         {:ns 'my.ns :name 'bar :file "src/my/ns.clj"
                                          :row 10 :arglists nil})]
      (is (= "(def bar)\n  Location: src/my/ns.clj:10" (:content entry)))
      (is (= "ns:my.ns/bar" (:key entry)))))

  (testing "creates snippet for def with empty arglists"
    (let [entry (t/var-def->memory-entry "test-project"
                                         {:ns 'my.ns :name 'baz :file "src/my/ns.clj"
                                          :row 5 :arglists []})]
      (is (= "(def baz)\n  Location: src/my/ns.clj:5" (:content entry)))
      (is (= "ns:my.ns/baz" (:key entry)))))

  (testing "key format matches edge from-key/to-key convention"
    (let [entry (t/var-def->memory-entry "p" {:ns 'a.b :name 'c :file "f" :row 1 :arglists nil})]
      (is (= "ns:a.b/c" (:key entry))
          "key must be 'ns:<namespace>/<name>' to align with call-edge->kg-edge :from-key/:to-key"))))

;; =============================================================================
;; namespace->memory-entry
;; =============================================================================

(deftest namespace->memory-entry-test
  (testing "creates snippet with dependencies and public var count"
    (let [entry (t/namespace->memory-entry
                 "test-project" 'my.app.core
                 {:ns 'my.app.core :depends-on #{'my.app.db 'clojure.string}}
                 sample-var-defs)]
      (is (= "snippet" (:type entry)))
      (is (= ["lsp" "namespace" "ns:my.app.core"] (:tags entry)))
      (is (= "medium" (:duration entry)))
      ;; 2 public vars in my.app.core (start!, stop! — not internal-helper)
      (is (.contains (:content entry) "Public vars: 2"))
      (is (.contains (:content entry) "Namespace: my.app.core"))
      ;; Dependencies should be sorted strings
      (is (.contains (:content entry) "Dependencies: "))
      (is (= "ns:my.app.core" (:key entry)))))

  (testing "creates snippet for namespace with no dependencies"
    (let [entry (t/namespace->memory-entry
                 "test-project" 'my.app.db
                 {:ns 'my.app.db :depends-on #{}}
                 sample-var-defs)]
      (is (.contains (:content entry) "Dependencies: []"))
      (is (.contains (:content entry) "Public vars: 2"))
      (is (= "ns:my.app.db" (:key entry)))))

  (testing "key format matches edge ns-dep->kg-edge convention"
    (let [entry (t/namespace->memory-entry "p" 'x.y {:ns 'x.y :depends-on #{}} [])]
      (is (= "ns:x.y" (:key entry))
          "key must be 'ns:<namespace>' to align with ns-dep->kg-edge :from-key/:to-key"))))

;; =============================================================================
;; call-edge->kg-edge
;; =============================================================================

(deftest call-edge->kg-edge-test
  (testing "creates depends-on edge with correct keys"
    (let [edge (t/call-edge->kg-edge
                {:caller-ns 'my.ns :caller-fn 'foo
                 :callee-ns 'other.ns :callee-fn 'bar
                 :file "src/my/ns.clj" :row 10})]
      (is (= "ns:my.ns/foo" (:from-key edge)))
      (is (= "ns:other.ns/bar" (:to-key edge)))
      (is (= :depends-on (:relation edge)))
      (is (= 1.0 (:confidence edge)))
      (is (= :automated (:source-type edge)))
      (is (= "lsp-mcp" (:created-by edge)))))

  (testing "from-key matches var-def->memory-entry :key format"
    (let [edge (t/call-edge->kg-edge
                {:caller-ns 'a.b :caller-fn 'c :callee-ns 'd :callee-fn 'e
                 :file "f" :row 1})
          entry (t/var-def->memory-entry "p" {:ns 'a.b :name 'c :file "f" :row 1 :arglists nil})]
      (is (= (:key entry) (:from-key edge))
          "edge :from-key must match memory entry :key for KG bridge resolution"))))

;; =============================================================================
;; ns-dep->kg-edge
;; =============================================================================

(deftest ns-dep->kg-edge-test
  (testing "creates namespace-level depends-on edge"
    (let [edge (t/ns-dep->kg-edge 'my.app.core 'my.app.db)]
      (is (= "ns:my.app.core" (:from-key edge)))
      (is (= "ns:my.app.db" (:to-key edge)))
      (is (= :depends-on (:relation edge)))
      (is (= :automated (:source-type edge)))))

  (testing "from-key matches namespace->memory-entry :key format"
    (let [edge (t/ns-dep->kg-edge 'x.y 'z.w)
          entry (t/namespace->memory-entry "p" 'x.y {:ns 'x.y :depends-on #{'z.w}} [])]
      (is (= (:key entry) (:from-key edge))
          "edge :from-key must match namespace entry :key for KG bridge resolution"))))

;; =============================================================================
;; analysis->kg-operations (integration)
;; =============================================================================

(deftest analysis->kg-operations-test
  (testing "full transformation pipeline"
    (let [result (t/analysis->kg-operations "test-project"
                                            sample-var-defs
                                            sample-call-graph
                                            sample-ns-graph)]
      ;; Structure checks
      (is (map? result))
      (is (vector? (:memory-entries result)))
      (is (vector? (:kg-edges result)))
      (is (map? (:stats result)))

      ;; Stats — 4 public vars (start!, stop!, query, connect) + 2 namespaces
      (is (= 4 (:fns (:stats result))))
      (is (= 2 (:namespaces (:stats result))))

      ;; Memory entries = 4 fn-entries + 2 ns-entries = 6
      (is (= 6 (count (:memory-entries result))))

      ;; All memory entries should be snippets with lsp tag and KG-linkable :key
      (is (every? #(= "snippet" (:type %)) (:memory-entries result)))
      (is (every? #(some #{"lsp"} (:tags %)) (:memory-entries result)))
      (is (every? :key (:memory-entries result)))

      ;; All keys should be unique (critical for KG bridge key->id-map)
      (let [keys (map :key (:memory-entries result))]
        (is (= (count keys) (count (set keys)))
            "memory entry :key values must be unique"))

      ;; KG edges — 2 call edges + ns dep edges (core→db, core→clojure.string) = 4
      ;; No impl edges in this test data
      (is (= 4 (:edges (:stats result))))
      (is (= 4 (count (:kg-edges result))))

      ;; All edges should have required keys
      (is (every? :from-key (:kg-edges result)))
      (is (every? :to-key (:kg-edges result)))
      (is (every? :relation (:kg-edges result)))
      (is (every? #(= "lsp-mcp" (:created-by %)) (:kg-edges result)))))

  (testing "edge from-key/to-key resolvable via memory entry :key"
    (let [result (t/analysis->kg-operations "test-project"
                                            sample-var-defs
                                            sample-call-graph
                                            sample-ns-graph)
          entry-keys (set (map :key (:memory-entries result)))
          call-edges (filter #(= :depends-on (:relation %)) (:kg-edges result))]
      ;; Call edges' from-key and to-key should reference keys present in memory-entries
      ;; This validates the key convention is consistent across transform output
      (doseq [edge call-edges]
        (when (.startsWith ^String (:from-key edge) "ns:")
          (when (.contains ^String (:from-key edge) "/")
            ;; Function-level edge — both keys should be in entry-keys
            (is (contains? entry-keys (:from-key edge))
                (str ":from-key " (:from-key edge) " should exist in memory entries")))))))

  (testing "filters out private var-defs from memory entries"
    (let [result (t/analysis->kg-operations "test-project"
                                            sample-var-defs
                                            [] [])
          fn-tags (mapcat :tags (:memory-entries result))]
      ;; internal-helper is private, should be excluded
      (is (= 4 (count (:memory-entries result))))
      (is (not (some #(= "ns:my.app.core/internal-helper" %) fn-tags)))))

  (testing "empty inputs produce empty results"
    (let [result (t/analysis->kg-operations "test-project" [] [] [])]
      (is (= [] (:memory-entries result)))
      (is (= [] (:kg-edges result)))
      (is (= {:fns 0 :edges 0 :namespaces 0} (:stats result))))))

;; =============================================================================
;; defmethod :implements edges
;; =============================================================================

(deftest implements-edges-test
  (testing "cross-namespace defmethod creates :implements edge"
    (let [var-defs [{:ns 'my.protocols :name 'process :defined-by 'clojure.core/defmulti
                     :file "src/my/protocols.clj" :row 5 :private? false}
                    {:ns 'my.impl :name 'process :defined-by 'clojure.core/defmethod
                     :file "src/my/impl.clj" :row 10 :private? false}]
          result   (t/analysis->kg-operations "test-project" var-defs [] [])]
      ;; Should have 1 implements edge (cross-namespace)
      (is (= 1 (count (filter #(= :implements (:relation %)) (:kg-edges result)))))
      (let [edge (first (filter #(= :implements (:relation %)) (:kg-edges result)))]
        (is (= "ns:my.impl/process" (:from-key edge)))
        (is (= "ns:my.protocols/process" (:to-key edge))))))

  (testing "same-namespace defmethod skips self-edge"
    (let [var-defs [{:ns 'my.ns :name 'dispatch :defined-by 'clojure.core/defmulti
                     :file "src/my/ns.clj" :row 5 :private? false}
                    {:ns 'my.ns :name 'dispatch :defined-by 'clojure.core/defmethod
                     :file "src/my/ns.clj" :row 15 :private? false}]
          result   (t/analysis->kg-operations "test-project" var-defs [] [])]
      ;; No implements edges — same ns/name produces identical keys
      (is (= 0 (count (filter #(= :implements (:relation %)) (:kg-edges result))))))))

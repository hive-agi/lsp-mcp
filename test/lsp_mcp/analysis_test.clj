(ns lsp-mcp.analysis-test
  (:require [clojure.test :refer [deftest is testing]]
            [lsp-mcp.analysis :as analysis]
            [lsp-mcp.cache :as cache]))

;; =============================================================================
;; Extract — var-definitions
;; =============================================================================

(deftest test-extract-var-definitions
  (testing "extracts var definitions from file URIs, filters jar URIs"
    (let [mock-analysis {"file:///src/my/ns.clj" {:var-definitions [{:ns 'my.ns
                                                                     :name 'foo
                                                                     :row 1
                                                                     :col 1
                                                                     :arglist-strs [[] ['x]]
                                                                     :private false
                                                                     :macro false
                                                                     :defined-by 'clojure.core/fn}]}
                         "jar://some.jar!path.clj" {:var-definitions [{:ns 'jar.ns
                                                                       :name 'jar-fn}]}}]
      (is (= [{:ns 'my.ns
               :name 'foo
               :file "file:///src/my/ns.clj"
               :row 1
               :col 1
               :arglists [[] ['x]]
               :private? false
               :macro? false
               :defined-by 'clojure.core/fn}]
             (analysis/extract-var-definitions mock-analysis)))))

  (testing "extracts from multiple file URIs"
    (let [mock-analysis {"file:///src/a.clj" {:var-definitions [{:ns 'a :name 'x :row 1 :col 1}]}
                         "file:///src/b.clj" {:var-definitions [{:ns 'b :name 'y :row 2 :col 1}
                                                                {:ns 'b :name 'z :row 5 :col 1}]}}
          result (analysis/extract-var-definitions mock-analysis)]
      (is (= 3 (count result)))
      (is (= #{'a 'b} (set (map :ns result))))))

  (testing "handles file URI with no var-definitions bucket"
    (let [mock-analysis {"file:///src/empty.clj" {:var-definitions []}}]
      (is (= [] (analysis/extract-var-definitions mock-analysis)))))

  (testing "normalizes missing arglist-strs to empty vector"
    (let [mock-analysis {"file:///src/my/ns.clj" {:var-definitions [{:ns 'my.ns
                                                                     :name 'bar
                                                                     :row 5
                                                                     :col 1}]}}
          result (first (analysis/extract-var-definitions mock-analysis))]
      (is (= [] (:arglists result)))))

  (testing "returns empty vector for empty analysis"
    (is (= [] (analysis/extract-var-definitions {})))))

;; =============================================================================
;; Extract — call-graph
;; =============================================================================

(deftest test-extract-call-graph
  (testing "extracts call graph from var usages with from-var, filters jar URIs"
    (let [mock-analysis {"file:///src/my/ns.clj" {:var-usages [{:name 'callee-fn
                                                                :from 'caller-ns
                                                                :from-var 'caller-fn
                                                                :to 'callee-ns
                                                                :row 5}]}
                         "jar://jar!path.clj" {:var-usages [{:name 'jar-callee
                                                             :from 'some-ns
                                                             :from-var 'some-fn
                                                             :to 'jar-ns}]}}]
      (is (= [{:caller-ns 'caller-ns
               :caller-fn 'caller-fn
               :callee-ns 'callee-ns
               :callee-fn 'callee-fn
               :file "file:///src/my/ns.clj"
               :row 5}]
             (analysis/extract-call-graph mock-analysis)))))

  (testing "extracts multiple call edges from same file"
    (let [mock-analysis {"file:///src/my/ns.clj"
                         {:var-usages [{:name 'a :from 'ns1 :from-var 'fn1 :to 'ns2 :row 1}
                                       {:name 'b :from 'ns1 :from-var 'fn1 :to 'ns3 :row 2}]}}
          result (analysis/extract-call-graph mock-analysis)]
      (is (= 2 (count result)))
      (is (= #{'ns2 'ns3} (set (map :callee-ns result))))))

  (testing "ignores usages without from-var"
    (let [mock-analysis {"file:///file.clj" {:var-usages [{:name 'fn
                                                           :to 'ns}]}}]
      (is (= [] (analysis/extract-call-graph mock-analysis)))))

  (testing "returns empty vector for empty analysis"
    (is (= [] (analysis/extract-call-graph {})))))

;; =============================================================================
;; Extract — namespace-graph
;; =============================================================================

(deftest test-extract-namespace-graph
  (testing "extracts namespace graph from dep-graph"
    (let [mock-dep-graph {'my.ns {:dependencies {'dep1 1 'dep2 2}
                                  :dependents {'dependent1 1}
                                  :internal? false}}]
      (is (= [{:ns 'my.ns
               :depends-on #{'dep1 'dep2}
               :dependents #{'dependent1}
               :internal? false}]
             (analysis/extract-namespace-graph mock-dep-graph)))))

  (testing "extracts multiple namespaces"
    (let [mock-dep-graph {'ns.a {:dependencies {'ns.b 1} :dependents {} :internal? true}
                          'ns.b {:dependencies {} :dependents {'ns.a 1} :internal? true}}
          result (analysis/extract-namespace-graph mock-dep-graph)]
      (is (= 2 (count result)))
      (is (= #{'ns.a 'ns.b} (set (map :ns result))))))

  (testing "handles empty dependencies and dependents"
    (let [mock-dep-graph {'lonely.ns {:dependencies {} :dependents {} :internal? false}}
          result (first (analysis/extract-namespace-graph mock-dep-graph))]
      (is (= #{} (:depends-on result)))
      (is (= #{} (:dependents result)))))

  (testing "returns empty vector for empty dep-graph"
    (is (= [] (analysis/extract-namespace-graph {})))))

;; =============================================================================
;; analyze-project! — integration with mocked cache
;; =============================================================================

(def ^:private sample-cached-analysis
  {:analysis {"file:///src/test/ns.clj"
              {:var-definitions [{:ns   'test.ns
                                  :name 'example
                                  :row  1
                                  :col  1}]}}
   :dep-graph {'test.ns {:dependencies {'clojure.core 1}
                         :dependents   {}
                         :internal?    true}}})

(deftest test-analyze-project!-cache-hit
  (testing "returns cached analysis when cache has fresh data"
    (with-redefs [cache/read-analysis (fn [_project-id] sample-cached-analysis)]
      (let [result (analysis/analyze-project! "/tmp/fake-project")]
        (is (map? result))
        (is (contains? result :analysis))
        (is (contains? result :dep-graph))
        (is (= sample-cached-analysis result))))))

(deftest test-analyze-project!-cache-miss-no-lsp
  (testing "returns error map when cache misses and clojure-lsp not on classpath"
    (with-redefs [cache/read-analysis (fn [_project-id] nil)]
      (let [result (analysis/analyze-project! "/tmp/fake-project")]
        (is (map? result))
        (is (string? (:error result)))
        (is (.contains ^String (:error result) "fake-project"))))))

(deftest test-analyze-project!-derives-project-id-from-path
  (testing "project-id is basename of project-root path"
    (let [queried-id (atom nil)]
      (with-redefs [cache/read-analysis (fn [project-id]
                                          (reset! queried-id project-id)
                                          sample-cached-analysis)]
        (analysis/analyze-project! "/home/user/projects/my-cool-project")
        (is (= "my-cool-project" @queried-id))))))

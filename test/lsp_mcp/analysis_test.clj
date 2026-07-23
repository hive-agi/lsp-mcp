(ns lsp-mcp.analysis-test
  "Unit tests for lsp-mcp.analysis.

   analyze-project! now returns a hive-dsl Result:
     {:ok analysis-map}      on success
     {:error category ...}   on failure"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [lsp-mcp.analysis :as analysis]
            [lsp-mcp.cache :as cache]
            [lsp-mcp.sidecar :as sidecar]))

;; =============================================================================
;; analyze-project! — input guard (railway-oriented)
;; =============================================================================

(deftest test-analyze-project!-blank-root-returns-err
  (testing "nil project-root => err :analysis/missing-root"
    (let [result (analysis/analyze-project! nil)]
      (is (r/err? result))
      (is (= :analysis/missing-root (:error result)))))

  (testing "empty project-root => err"
    (is (r/err? (analysis/analyze-project! ""))))

  (testing "whitespace-only project-root => err"
    (is (r/err? (analysis/analyze-project! "   ")))
    (is (r/err? (analysis/analyze-project! "\t\n")))))

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
;; analyze-project! — integration with mocked cache (Result-aware)
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
  (testing "returns ok Result wrapping cached analysis when cache fresh"
    (with-redefs [cache/project-id-for (constantly "fake-project")
                  cache/read-analysis
                  (fn [_project-id] sample-cached-analysis)]
      (let [result (analysis/analyze-project! "/tmp/fake-project")]
        (is (r/ok? result))
        (is (= sample-cached-analysis (:ok result)))))))

(deftest test-analyze-project!-cache-miss-no-lsp
  (testing "returns err Result with structured fix info when all sources fail"
    (with-redefs [cache/project-id-for (constantly "fake-project")
                  cache/read-analysis (fn [_project-id] nil)
                  lsp-mcp.sidecar/ensure-analysis!
                  (fn [_pid]
                    {:error :analysis/sidecar-unavailable
                     :fix :start-sidecar
                     :command "docker compose up -d lsp-sidecar"
                     :message "mock sidecar unavailable"})]
      (let [result (analysis/analyze-project! "/tmp/fake-project")]
        (is (r/err? result))
        (is (#{:analysis/lsp-unavailable :analysis/dump-failed
               :analysis/sidecar-unavailable :analysis/sidecar-timeout}
             (:error result)))
        (is (keyword? (:fix result))
            "Error result must include :fix key for self-remediation"))))
  (testing "structured error on missing-root includes :fix :init-project"
    (let [result (analysis/analyze-project! nil)]
      (is (= :analysis/missing-root (:error result)))
      (is (= :init-project (:fix result)))
      (is (string? (:hint result))))))

(deftest test-analyze-project!-derives-project-id-from-path
  (testing "uses the workspace-relative project-id supplied by the cache bridge"
    (let [queried-id (atom nil)
          project-root "/home/user/projects/my-cool-project"]
      (with-redefs [cache/project-id-for
                    (fn [root]
                      (is (= project-root root))
                      "projects/my-cool-project")
                    cache/read-analysis
                    (fn [project-id]
                      (reset! queried-id project-id)
                      sample-cached-analysis)]
        (analysis/analyze-project! project-root)
        (is (= "projects/my-cool-project" @queried-id))))))

;; =============================================================================
;; analyze-project! — :local/root pre-flight (sidecar path)
;; =============================================================================

(defn- with-temp-deps-edn
  "Write `content` to a fresh temp project root, call `f` with that
   path, then clean up. Returns the value f returned."
  [content f]
  (let [dir (java.nio.file.Files/createTempDirectory
             "lsp-mcp-deps-test"
             (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (spit (str dir "/deps.edn") content)
      (f (str dir))
      (finally
        (try (java.nio.file.Files/delete (.resolve dir "deps.edn")) (catch Exception _))
        (try (java.nio.file.Files/delete dir) (catch Exception _))))))

(deftest test-analyze-project!-local-root-unreachable-fails-fast
  (testing "deps.edn with :local/root pointing outside workspace returns ELM error"
    (with-temp-deps-edn
      "{:deps {a/b {:local/root \"/nowhere/foreign\"}}}"
      (fn [project-root]
        (with-redefs [cache/read-analysis (fn [_] nil)
                      cache/workspace-root (fn [] "/strictly-isolated-workspace")
                      cache/project-id-for (fn [_] "fake-id")
                      sidecar/ensure-analysis! (fn [_]
                                                 (throw (ex-info "must-not-call-sidecar" {})))]
          (let [result (analysis/analyze-project! project-root)]
            (is (r/err? result))
            (is (= :analysis/local-root-deps-unreachable (:error result)))
            (is (= :align-local-roots (:fix result)))
            (is (= 1 (count (:unreachable result))))
            (is (= 'a/b (:dep (first (:unreachable result)))))
            (is (string? (:hint result)))
            (is (clojure.string/includes? (:hint result) "/nowhere/foreign"))
            (is (clojure.string/includes? (:hint result) "the sidecar container"))
            (is (clojure.string/includes? (:hint result) "WRONG:"))
            (is (clojure.string/includes? (:hint result) "RIGHT:"))))))))

(deftest test-analyze-project!-local-root-reachable-passes
  (testing "deps.edn with reachable :local/root falls through to sidecar"
    (with-temp-deps-edn
      "{:deps {a/b {:mvn/version \"1.0\"}}}"
      (fn [project-root]
        (with-redefs [cache/read-analysis (fn [_] nil)
                      cache/workspace-root (fn [] project-root)
                      cache/project-id-for (fn [_] "fake-id")
                      sidecar/ensure-analysis! (fn [_]
                                                 {:error :analysis/sidecar-unavailable
                                                  :message "passthrough"})]
          (let [result (analysis/analyze-project! project-root)]
            (is (r/err? result))
            ;; The pre-flight passed (no :local/root deps at all).
            ;; Result should reflect downstream sidecar error, not validate.
            (is (= :analysis/sidecar-unavailable (:error result)))))))))


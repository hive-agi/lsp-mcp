(ns lsp-mcp.tools-test
  "Tests for lsp-mcp.tools — MCP tool handler and dispatch."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.edn :as edn]
            [lsp-mcp.tools :as tools]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def sample-analysis
  "Minimal analysis result matching what core/analyze returns."
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
                                  :defined-by 'clojure.core/defn}]
               :var-usages [{:from 'my.app.core :from-var 'start!
                             :to 'my.app.db :name 'connect
                             :row 12}]}
              "file://src/my/app/db.clj"
              {:var-definitions [{:ns 'my.app.db :name 'connect
                                  :row 5 :col 1
                                  :arglist-strs nil
                                  :private false :macro false
                                  :defined-by 'clojure.core/def}]
               :var-usages []}}
   :dep-graph {'my.app.core {:dependencies {'my.app.db 1}
                             :dependents   {}
                             :internal?    true}
               'my.app.db   {:dependencies {}
                             :dependents   {'my.app.core 1}
                             :internal?    true}}})

(defn mock-analyze-fixture
  "Test fixture that mocks core/analyze to return sample-analysis
   and clears the tools memoization cache before/after each test."
  [f]
  (tools/invalidate-cache!)
  (with-redefs [lsp-mcp.core/analyze (constantly sample-analysis)]
    (f))
  (tools/invalidate-cache!))

(use-fixtures :each mock-analyze-fixture)

;; =============================================================================
;; Helper
;; =============================================================================

(defn- parse-response
  "Parse the MCP response, extracting the EDN from the first content text."
  [response]
  (edn/read-string (get-in response [:content 0 :text])))

;; =============================================================================
;; handle-lsp dispatch tests
;; =============================================================================

(deftest handle-lsp-analyze-test
  (testing "analyze command returns stats"
    (let [resp   (tools/handle-lsp {:command "analyze" :project_root "/test"})
          result (parse-response resp)]
      (is (nil? (:isError resp)))
      (is (= 2 (:num-files result)))
      (is (= 2 (:num-namespaces result)))
      (is (= 3 (:num-vars result)))
      (is (map? (:cache-status result))))))

(deftest handle-lsp-definitions-test
  (testing "definitions command returns all var defs"
    (let [result (parse-response (tools/handle-lsp {:command "definitions" :project_root "/test"}))]
      (is (= 3 (count result)))
      (is (every? :ns result))
      (is (every? :name result))))

  (testing "definitions with namespace filter"
    (let [result (parse-response (tools/handle-lsp {:command      "definitions"
                                                    :project_root "/test"
                                                    :namespace    "my.app.core"}))]
      (is (= 2 (count result)))
      (is (every? #(= 'my.app.core (:ns %)) result)))))

(deftest handle-lsp-calls-test
  (testing "calls command returns call graph edges"
    (let [result (parse-response (tools/handle-lsp {:command "calls" :project_root "/test"}))]
      (is (= 1 (count result)))
      (is (= 'my.app.core (:caller-ns (first result))))
      (is (= 'connect (:callee-fn (first result))))))

  (testing "calls with namespace filter"
    (let [result (parse-response (tools/handle-lsp {:command      "calls"
                                                    :project_root "/test"
                                                    :namespace    "my.app.core"}))]
      (is (= 1 (count result)))))

  (testing "calls with non-matching namespace filter returns empty"
    (let [result (parse-response (tools/handle-lsp {:command      "calls"
                                                    :project_root "/test"
                                                    :namespace    "nonexistent"}))]
      (is (empty? result)))))

(deftest handle-lsp-ns-graph-test
  (testing "ns-graph command returns namespace dependency graph"
    (let [result (parse-response (tools/handle-lsp {:command "ns-graph" :project_root "/test"}))]
      (is (= 2 (count result)))
      (is (every? :ns result))
      (is (every? :depends-on result)))))

(deftest handle-lsp-callers-test
  (testing "callers command returns callers of a function"
    (let [result (parse-response (tools/handle-lsp {:command      "callers"
                                                    :project_root "/test"
                                                    :function     "connect"}))]
      (is (= 1 (count result)))
      (is (= 'my.app.core (:caller-ns (first result))))))

  (testing "callers with namespace filter"
    (let [result (parse-response (tools/handle-lsp {:command      "callers"
                                                    :project_root "/test"
                                                    :namespace    "my.app.db"
                                                    :function     "connect"}))]
      (is (= 1 (count result))))))

(deftest handle-lsp-references-test
  (testing "references command returns reference locations"
    (let [result (parse-response (tools/handle-lsp {:command      "references"
                                                    :project_root "/test"
                                                    :function     "connect"}))]
      (is (= 1 (count result)))
      (let [ref (first result)]
        (is (:file ref))
        (is (:row ref))
        (is (:caller-ns ref))
        (is (:caller-fn ref))))))

(deftest handle-lsp-status-test
  (testing "status command returns bridge and cache info"
    (let [result (parse-response (tools/handle-lsp {:command "status"}))]
      (is (contains? result :bridge-available?))
      (is (contains? result :cache)))))

;; =============================================================================
;; Error handling tests
;; =============================================================================

(deftest handle-lsp-unknown-command-test
  (testing "unknown command returns error with available commands"
    (let [resp   (tools/handle-lsp {:command "nonexistent"})
          result (parse-response resp)]
      (is (:isError resp))
      (is (= "Unknown command" (:error result)))
      (is (= "nonexistent" (:command result)))
      (is (seq (:available result))))))

(deftest handle-lsp-exception-test
  (testing "exception in handler returns error response"
    (with-redefs [lsp-mcp.core/analyze (fn [_] (throw (ex-info "Boom" {})))]
      (tools/invalidate-cache!)
      (let [resp   (tools/handle-lsp {:command "analyze" :project_root "/test"})
            result (parse-response resp)]
        (is (:isError resp))
        (is (= "Failed to handle command" (:error result)))
        (is (.contains (:details result) "Boom"))))))

(deftest handle-lsp-nil-command-test
  (testing "nil command returns error"
    (let [resp   (tools/handle-lsp {:command nil})
          result (parse-response resp)]
      (is (:isError resp))
      (is (= "Unknown command" (:error result))))))

;; =============================================================================
;; tool-def schema tests
;; =============================================================================

(deftest tool-def-test
  (testing "tool-def returns valid MCP tool definition"
    (let [td (tools/tool-def)]
      (is (= "lsp" (:name td)))
      (is (string? (:description td)))
      (is (map? (:inputSchema td)))))

  (testing "tool-def inputSchema has required command property"
    (let [schema (:inputSchema (tools/tool-def))]
      (is (= "object" (:type schema)))
      (is (= ["command"] (:required schema)))
      (is (get-in schema [:properties :command :enum]))))

  (testing "tool-def enum matches command-handlers keys"
    (let [enum (get-in (tools/tool-def) [:inputSchema :properties :command :enum])]
      (is (= (sort enum) enum) "enum should be sorted")
      (is (some #{"analyze"} enum))
      (is (some #{"definitions"} enum))
      (is (some #{"calls"} enum))
      (is (some #{"ns-graph"} enum))
      (is (some #{"sync"} enum))
      (is (some #{"status"} enum))
      (is (some #{"callers"} enum))
      (is (some #{"references"} enum)))))

;; =============================================================================
;; Nil project_root validation tests
;; =============================================================================

(deftest handle-lsp-definitions-nil-project-root-test
  (testing "definitions without project_root returns error (not NPE)"
    (let [resp   (tools/handle-lsp {:command "definitions"})
          result (parse-response resp)]
      (is (:isError resp))
      (is (string? (:error result)))
      (is (re-find #"project_root" (:error result)))))

  (testing "definitions with blank project_root returns error"
    (let [resp   (tools/handle-lsp {:command "definitions" :project_root ""})
          result (parse-response resp)]
      (is (:isError resp))
      (is (string? (:error result))))))

(deftest handle-lsp-callers-nil-project-root-test
  (testing "callers without project_root returns error"
    (let [resp   (tools/handle-lsp {:command "callers"})
          result (parse-response resp)]
      (is (:isError resp))
      (is (string? (:error result)))
      (is (re-find #"project_root" (:error result))))))

(deftest handle-lsp-callers-no-function-test
  (testing "callers without function or namespace returns all call edges"
    (let [result (parse-response (tools/handle-lsp {:command      "callers"
                                                    :project_root "/test"}))]
      (is (vector? result))
      (is (= 1 (count result))))))

(deftest handle-lsp-analyze-nil-project-root-test
  (testing "analyze without project_root returns error"
    (let [resp   (tools/handle-lsp {:command "analyze"})
          result (parse-response resp)]
      (is (:isError resp))
      (is (string? (:error result))))))

(deftest handle-lsp-calls-nil-project-root-test
  (testing "calls without project_root returns error"
    (let [resp   (tools/handle-lsp {:command "calls"})
          result (parse-response resp)]
      (is (:isError resp))
      (is (string? (:error result))))))

(deftest handle-lsp-ns-graph-nil-project-root-test
  (testing "ns-graph without project_root returns error"
    (let [resp   (tools/handle-lsp {:command "ns-graph"})
          result (parse-response resp)]
      (is (:isError resp))
      (is (string? (:error result))))))

(deftest handle-lsp-references-nil-project-root-test
  (testing "references without project_root returns error"
    (let [resp   (tools/handle-lsp {:command "references"})
          result (parse-response resp)]
      (is (:isError resp))
      (is (string? (:error result))))))

(deftest handle-lsp-sync-nil-project-root-test
  (testing "sync without project_root returns error"
    (let [resp   (tools/handle-lsp {:command "sync"})
          result (parse-response resp)]
      (is (:isError resp))
      (is (string? (:error result))))))

;; =============================================================================
;; Memoization tests
;; =============================================================================

(deftest cached-analyze-test
  (testing "multiple commands within TTL share the same analyze call"
    (let [call-count (atom 0)]
      (with-redefs [lsp-mcp.core/analyze (fn [_]
                                           (swap! call-count inc)
                                           sample-analysis)]
        (tools/invalidate-cache!)
        ;; Three rapid commands for the same project
        (tools/handle-lsp {:command "analyze" :project_root "/test"})
        (tools/handle-lsp {:command "definitions" :project_root "/test"})
        (tools/handle-lsp {:command "calls" :project_root "/test"})
        ;; core/analyze should only be called once due to memoization
        (is (= 1 @call-count)))))

  (testing "different project_root triggers new analysis"
    (let [call-count (atom 0)]
      (with-redefs [lsp-mcp.core/analyze (fn [_]
                                           (swap! call-count inc)
                                           sample-analysis)]
        (tools/invalidate-cache!)
        (tools/handle-lsp {:command "analyze" :project_root "/project-a"})
        (tools/handle-lsp {:command "analyze" :project_root "/project-b"})
        (is (= 2 @call-count)))))

  (testing "invalidate-cache! forces re-analysis"
    (let [call-count (atom 0)]
      (with-redefs [lsp-mcp.core/analyze (fn [_]
                                           (swap! call-count inc)
                                           sample-analysis)]
        (tools/invalidate-cache!)
        (tools/handle-lsp {:command "analyze" :project_root "/test"})
        (tools/invalidate-cache!)
        (tools/handle-lsp {:command "analyze" :project_root "/test"})
        (is (= 2 @call-count))))))

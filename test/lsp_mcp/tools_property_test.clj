(ns lsp-mcp.tools-property-test
  "Property-based tests for lsp-mcp.tools validation guards.

   Properties tested:
   - Totality: handle-lsp never throws NPE for any project_root input
   - Complement: blank project_root => isError, non-blank => no isError
   - Idempotent: same nil input always produces same error shape"
  (:require [clojure.test :refer [use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.properties :as props]
            [hive-test.generators.core :as gen-core]
            [lsp-mcp.tools :as tools]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(def sample-analysis
  {:analysis {"file://src/a.clj"
              {:var-definitions [{:ns 'a :name 'f :row 1 :col 1}]
               :var-usages [{:from 'a :from-var 'f :to 'b :name 'g :row 2}]}}
   :dep-graph {'a {:dependencies {'b 1} :dependents {} :internal? true}}})

(defn mock-fixture [f]
  (tools/invalidate-cache!)
  (with-redefs [lsp-mcp.core/analyze (constantly sample-analysis)]
    (f))
  (tools/invalidate-cache!))

(use-fixtures :each mock-fixture)

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-project-root-requiring-command
  "Commands that require project_root."
  (gen/elements ["analyze" "definitions" "calls" "ns-graph"
                 "callers" "references" "sync"]))

(def gen-blank-string
  "Generator for nil or blank strings."
  (gen/one-of [(gen/return nil)
               (gen/return "")
               (gen/return "   ")
               (gen/return "\t\n")]))

(def gen-valid-project-root
  "Generator for non-blank project root paths."
  (gen/fmap #(str "/tmp/project-" %) gen-core/gen-non-blank-string))

(def gen-handle-lsp-params-no-root
  "Params with a project_root-requiring command but nil/blank project_root."
  (gen/let [cmd gen-project-root-requiring-command
            root gen-blank-string]
    {:command cmd :project_root root}))

(def gen-handle-lsp-params-with-root
  "Params with a project_root-requiring command and valid project_root."
  (gen/let [cmd gen-project-root-requiring-command
            root gen-valid-project-root]
    {:command cmd :project_root root}))

;; =============================================================================
;; P1 — Totality: handle-lsp never throws for blank project_root
;; =============================================================================

(props/defprop-total p1-handle-lsp-nil-root-total
  tools/handle-lsp gen-handle-lsp-params-no-root
  {:num-tests 100 :pred map?})

;; =============================================================================
;; P2 — Totality: handle-lsp never throws for valid project_root
;; =============================================================================

(props/defprop-total p2-handle-lsp-valid-root-total
  tools/handle-lsp gen-handle-lsp-params-with-root
  {:num-tests 100 :pred map?})

;; =============================================================================
;; P3 — Complement: blank root => isError, non-blank root => no isError
;; =============================================================================

(defspec p3-blank-root-always-error 100
  (prop/for-all [params gen-handle-lsp-params-no-root]
                (let [resp (tools/handle-lsp params)]
                  (true? (:isError resp)))))

(defspec p4-valid-root-never-validation-error 100
  (prop/for-all [params gen-handle-lsp-params-with-root]
                (let [resp (tools/handle-lsp params)]
      ;; sync may produce domain errors, but never a "project_root is required" validation error
                  (let [text (get-in resp [:content 0 :text] "")]
                    (not (re-find #"project_root is required" text))))))

;; =============================================================================
;; P5 — Idempotent: same nil input always produces identical error response
;; =============================================================================

(defspec p5-nil-root-error-idempotent 50
  (prop/for-all [cmd gen-project-root-requiring-command]
                (let [params {:command cmd}
                      resp1  (tools/handle-lsp params)
                      resp2  (tools/handle-lsp params)]
                  (= resp1 resp2))))

;; =============================================================================
;; P6 — Error shape: all validation errors have consistent structure
;; =============================================================================

(defspec p6-error-shape-consistent 100
  (prop/for-all [params gen-handle-lsp-params-no-root]
                (let [resp (tools/handle-lsp params)
                      text (get-in resp [:content 0 :text] "")]
                  (and (:isError resp)
                       (vector? (:content resp))
                       (= 1 (count (:content resp)))
                       (= "text" (get-in resp [:content 0 :type]))
                       (string? text)
                       (re-find #"project_root" text)))))

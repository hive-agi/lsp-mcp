(ns lsp-mcp.analysis-property-test
  "Property-based tests for lsp-mcp.analysis nil guard.

   Properties tested:
   - Totality: analyze-project! never throws NPE for any input
   - Complement: blank input => error map, non-blank => no :error about project-root"
  (:require [clojure.test :refer [use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.properties :as props]
            [hive-test.generators.core :as gen-core]
            [lsp-mcp.analysis :as analysis]
            [lsp-mcp.cache :as cache]))

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-blank-input
  "nil or blank strings."
  (gen/one-of [(gen/return nil)
               (gen/return "")
               (gen/return "   ")
               (gen/return "\t\n")]))

(def gen-valid-project-root
  "Non-blank project root paths."
  (gen/fmap #(str "/tmp/project-" %) gen-core/gen-non-blank-string))

;; =============================================================================
;; P1 — Totality: analyze-project! never throws for blank input
;; =============================================================================

(props/defprop-total p1-analyze-nil-total
  analysis/analyze-project! gen-blank-input
  {:num-tests 100 :pred map?})

;; =============================================================================
;; P2 — Totality: analyze-project! never throws for valid input (with mock cache)
;; =============================================================================

(def sample-cached {:analysis {} :dep-graph {}})

(defspec p2-analyze-valid-total 50
  (prop/for-all [root gen-valid-project-root]
                (with-redefs [cache/read-analysis (constantly sample-cached)]
                  (let [result (analysis/analyze-project! root)]
                    (map? result)))))

;; =============================================================================
;; P3 — Complement: blank => error with "project-root", non-blank => no such error
;; =============================================================================

(defspec p3-blank-returns-project-root-error 100
  (prop/for-all [input gen-blank-input]
                (let [result (analysis/analyze-project! input)]
                  (and (map? result)
                       (string? (:error result))
                       (boolean (re-find #"project.root" (:error result)))))))

(defspec p4-valid-root-no-project-root-error 50
  (prop/for-all [root gen-valid-project-root]
                (with-redefs [cache/read-analysis (constantly sample-cached)]
                  (let [result (analysis/analyze-project! root)]
                    (not= "project-root is required for analysis" (:error result))))))

;; =============================================================================
;; P5 — Idempotent: blank input always produces same error
;; =============================================================================

(defspec p5-blank-error-idempotent 50
  (prop/for-all [input gen-blank-input]
                (= (analysis/analyze-project! input)
                   (analysis/analyze-project! input))))

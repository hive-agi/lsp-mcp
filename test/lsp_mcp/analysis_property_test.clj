(ns lsp-mcp.analysis-property-test
  "Property-based tests for lsp-mcp.analysis (Result-returning).

   Properties tested:
   - Totality: analyze-project! never throws — always returns a Result map
   - Complement: blank input => err Result; non-blank => ok or different err"
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as r]
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
;; P1 — Totality: analyze-project! always returns a Result map for blank input
;; =============================================================================

(props/defprop-total p1-analyze-nil-total
  analysis/analyze-project! gen-blank-input
  {:num-tests 100 :pred #(or (r/ok? %) (r/err? %))})

;; =============================================================================
;; P2 — Totality: analyze-project! always returns ok Result for valid input
;;       (with mocked cache)
;; =============================================================================

(def sample-cached {:analysis {} :dep-graph {}})

(defspec p2-analyze-valid-total 50
  (prop/for-all [root gen-valid-project-root]
                (with-redefs [cache/read-analysis (constantly sample-cached)]
                  (let [result (analysis/analyze-project! root)]
                    (r/ok? result)))))

;; =============================================================================
;; P3 — Complement: blank => :analysis/missing-root, non-blank => never
;; =============================================================================

(defspec p3-blank-returns-missing-root-err 100
  (prop/for-all [input gen-blank-input]
                (let [result (analysis/analyze-project! input)]
                  (and (r/err? result)
                       (= :analysis/missing-root (:error result))))))

(defspec p4-valid-root-no-missing-root-err 50
  (prop/for-all [root gen-valid-project-root]
                (with-redefs [cache/read-analysis (constantly sample-cached)]
                  (let [result (analysis/analyze-project! root)]
                    (not= :analysis/missing-root (:error result))))))

;; =============================================================================
;; P5 — Idempotent: blank input always produces same error
;; =============================================================================

(defspec p5-blank-error-idempotent 50
  (prop/for-all [input gen-blank-input]
                (= (analysis/analyze-project! input)
                   (analysis/analyze-project! input))))

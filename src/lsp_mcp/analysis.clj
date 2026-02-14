(ns lsp-mcp.analysis
  "Core extraction layer for clojure-lsp analysis data.

   Reads from Docker sidecar cache (preferred) or falls back to
   in-process clojure-lsp.api/dump when cache is unavailable.

   Extract functions work on the analysis data regardless of source."
  (:require
   [lsp-mcp.cache :as cache]
   [clojure.java.io :as io]
   [lsp-mcp.log :as log]))

;; =============================================================================
;; Analysis Source (Cache-First with In-Process Fallback)
;; =============================================================================

(defn analyze-project!
  "Analyze a project using clojure-lsp.

   Strategy:
   1. Try cached analysis from Docker sidecar (fast, no JVM cost)
   2. Fall back to in-process clojure-lsp.api/dump (if on classpath)
   3. Return error map if neither available

   project-root - string path to the project root directory."
  [project-root]
  (let [project-id (.getName (io/file project-root))]
    ;; Strategy 1: Cache from Docker sidecar
    (if-let [cached (cache/read-analysis project-id)]
      (do
        (log/info "Using cached analysis for" project-id)
        cached)
      ;; Strategy 2: In-process fallback
      (do
        (log/info "Cache miss for" project-id ", trying in-process clojure-lsp")
        (if-let [dump-fn (try (requiring-resolve 'clojure-lsp.api/dump)
                              (catch Exception _ nil))]
          (try
            (let [result (dump-fn
                          {:project-root (io/file project-root)
                           :output       {:filter-keys [:analysis :dep-graph]}
                           :analysis     {:type :project-only}})]
              (:result result))
            (catch Exception e
              (log/error "In-process analysis failed:" (ex-message e))
              {:error (ex-message e)}))
          (do
            (log/warn "No cache and clojure-lsp not on classpath")
            {:error (str "No analysis available for " project-id
                         ". Start the LSP sidecar or add clojure-lsp to classpath.")}))))))

;; =============================================================================
;; Extraction Helpers
;; =============================================================================

(defn- file-uri?
  "Returns true if uri starts with \"file://\" (not jar://)."
  [uri]
  (and (string? uri)
       (.startsWith ^String uri "file://")))

(defn extract-var-definitions
  "Extract all var definitions from an analysis map.
   Filters to file:// URIs only (excludes jar:// dependencies).

   analysis - map of {uri-string -> {:var-definitions [...] ...}}

   Returns vec of maps:
     {:ns sym, :name sym, :file str, :row int, :col int,
      :arglists vec, :private? bool, :macro? bool, :defined-by sym}"
  [analysis]
  (into []
        (comp
         (filter (fn [[uri _]] (file-uri? uri)))
         (mapcat (fn [[uri buckets]]
                   (map (fn [vd]
                          {:ns         (:ns vd)
                           :name       (:name vd)
                           :file       uri
                           :row        (:row vd)
                           :col        (:col vd)
                           :arglists   (or (:arglist-strs vd) [])
                           :private?   (boolean (:private vd))
                           :macro?     (boolean (:macro vd))
                           :defined-by (:defined-by vd)})
                        (:var-definitions buckets)))))
        analysis))

(defn extract-call-graph
  "Extract call graph edges from var-usages in the analysis map.
   Only includes edges where :from-var is present (calls from
   within a named function, not top-level).

   analysis - map of {uri-string -> {:var-usages [...] ...}}

   Returns vec of maps:
     {:caller-ns sym, :caller-fn sym, :callee-ns sym, :callee-fn sym,
      :file str, :row int}"
  [analysis]
  (into []
        (comp
         (filter (fn [[uri _]] (file-uri? uri)))
         (mapcat (fn [[uri buckets]]
                   (into []
                         (comp
                          (filter :from-var)
                          (map (fn [vu]
                                 {:caller-ns (:from vu)
                                  :caller-fn (:from-var vu)
                                  :callee-ns (:to vu)
                                  :callee-fn (:name vu)
                                  :file      uri
                                  :row       (:row vu)})))
                         (:var-usages buckets)))))
        analysis))

(defn extract-namespace-graph
  "Transform dep-graph into a normalized namespace dependency graph.

   dep-graph - map of {ns-sym -> {:dependencies {ns count}
                                   :dependents  {ns count}
                                   :internal?   bool ...}}

   Returns vec of maps:
     {:ns sym, :depends-on #{sym}, :dependents #{sym}, :internal? bool}"
  [dep-graph]
  (into []
        (map (fn [[ns-sym entry]]
               {:ns          ns-sym
                :depends-on  (set (keys (:dependencies entry)))
                :dependents  (set (keys (:dependents entry)))
                :internal?   (boolean (:internal? entry))}))
        dep-graph))

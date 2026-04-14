(ns lsp-mcp.analysis
  "Core extraction layer for clojure-lsp analysis data.

   Reads from Docker sidecar cache (preferred) or falls back to
   in-process clojure-lsp.api/dump when cache is unavailable.
   When both fail, triggers the sidecar on-demand and awaits results.

   Railway-oriented: analyze-project! returns a hive-dsl Result
   ({:ok ...} | {:error category ...data}). Extract fns are pure
   and operate on raw analysis maps."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hive-dsl.result :as r]
   [lsp-mcp.cache :as cache]
   [lsp-mcp.sidecar :as sidecar]
   [lsp-mcp.log :as log]))

;; =============================================================================
;; Analysis Source (Cache-First with In-Process Fallback) — Railway-Oriented
;; =============================================================================

(defn- try-cache
  "Try cached analysis from Docker sidecar. Returns Result.
   ok = cached map; err :analysis/cache-miss with structured fix info."
  [project-id]
  (if-let [cached (cache/read-analysis project-id)]
    (do (log/info "Using cached analysis for" project-id)
        (r/ok cached))
    (r/err :analysis/cache-miss
           {:project-id project-id
            :fix        :trigger-sidecar
            :command    "docker exec hive-mcp-lsp-sidecar kill -HUP 1"
            :hint       (str "No cached analysis for '" project-id "'. "
                             "Trigger sidecar or wait for next cycle.")
            :message    (str "Cache miss for project: " project-id)})))

(defn- try-in-process
  "Fallback to in-process clojure-lsp.api/dump. Returns Result.
   err :analysis/lsp-unavailable with structured fix if clojure-lsp not on classpath,
   err :analysis/dump-failed with structured fix on dump exception."
  [project-root]
  (if-let [dump-fn (try (requiring-resolve 'clojure-lsp.api/dump)
                        (catch Exception _ nil))]
    (r/try-effect* :analysis/dump-failed
      (let [result (dump-fn
                    {:project-root (io/file project-root)
                     :output       {:filter-keys [:analysis :dep-graph]}
                     :analysis     {:type :project-only}})]
        (:result result)))
    (r/err :analysis/lsp-unavailable
           {:fix     :trigger-sidecar
            :command "docker compose up -d lsp-sidecar"
            :hint    (str "clojure-lsp not on classpath. "
                          "Use the Docker sidecar for analysis, or add clojure-lsp as a dependency.")
            :message "clojure-lsp not on classpath"})))

(defn- try-sidecar-on-demand
  "Last resort: trigger the Docker sidecar to analyze on demand and await results.
   Returns Result — ok with analysis or err with structured timeout/unavailable info."
  [project-id]
  (log/info "Triggering on-demand sidecar analysis for" project-id)
  (let [result (sidecar/ensure-analysis! project-id)]
    (if (:ok result)
      (r/ok (:ok result))
      (r/err (:error result)
             (dissoc result :error)))))

(defn analyze-project!
  "Analyze a project using clojure-lsp. Returns a Result.

   Strategy:
   1. Try cached analysis from Docker sidecar (fast)
   2. Fall back to in-process clojure-lsp.api/dump
   3. Trigger sidecar on-demand and await (lazy spawn)
   4. Return structured err Result if all fail

   project-root - string path to project root.

   Returns:
     {:ok analysis-map}     on success
     {:error category :fix keyword :command str :hint str :message str}  on failure"
  [project-root]
  (if (str/blank? project-root)
    (r/err :analysis/missing-root
           {:fix     :init-project
            :hint    "Create deps.edn (or project.clj) at the project root directory."
            :command "echo '{:deps {}}' > deps.edn"
            :message "project-root is required for analysis"})
    (if-let [project-id (cache/project-id-for project-root)]
      (let [cached (try-cache project-id)]
        (if (r/ok? cached)
          cached
          (let [in-proc (try-in-process project-root)]
            (if (r/ok? in-proc)
              in-proc
              ;; Both cache and in-process failed — try on-demand sidecar
              (do (log/info "Cache miss + in-process unavailable for" project-id
                            ", triggering sidecar on-demand")
                  (try-sidecar-on-demand project-id))))))
      (r/err :analysis/outside-workspace
             {:project-root  project-root
              :workspace-root (cache/workspace-root)
              :fix           :widen-workspace-mount
              :command       (str "export HIVE_LSP_WORKSPACE_ROOT=<host-parent-dir> && "
                                  "docker compose -f hive-mcp/docker-compose.yml "
                                  "up -d --force-recreate lsp-sidecar")
              :hint          (str "project-root is outside the sidecar workspace mount ("
                                  (cache/workspace-root) "). "
                                  "Set HIVE_LSP_WORKSPACE_ROOT to a parent directory "
                                  "that contains both the workspace and the project, "
                                  "then restart the sidecar.")
              :message       (str "project-root " project-root
                                  " not under workspace-root " (cache/workspace-root))}))))

;; =============================================================================
;; Extraction Helpers (pure)
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

(ns lsp-mcp.cache
  "Reads cached clojure-lsp analysis from shared volume.

   The Docker sidecar container periodically runs clojure-lsp dump
   and writes EDN files to a shared cache directory. This namespace
   provides a thin bridge to read that cached data.

   Cache structure:
     <cache-dir>/<project-id>/dump.edn   - full analysis result
     <cache-dir>/<project-id>/meta.edn   - freshness metadata

   CLARITY-L: Pure read-only bridge layer, no domain logic.

   Config: cache-dir / workspace-root resolve through lsp-mcp.config
   (hive-di defconfig). Thin wrappers here preserve the existing API
   surface — callers already reach for lsp-mcp.cache/cache-dir."
  (:require
   [babashka.fs :as fs]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [lsp-mcp.config :as config]
   [lsp-mcp.log :as log]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:private default-max-age-ms
  "Default max age for cached analysis: 10 minutes."
  (* 10 60 1000))

(defn cache-dir
  "Resolve the LSP cache directory via lsp-mcp.config.
   Priority: LSP_CACHE_DIR env var > ~/.cache/hive-lsp."
  []
  (config/cache-dir))

(defn workspace-root
  "Host-side root that the sidecar mounts as /workspace.
   Must match the docker-compose HIVE_LSP_WORKSPACE_ROOT setting.
   Priority: HIVE_LSP_WORKSPACE_ROOT env var > ~/PP."
  []
  (config/workspace-root))

(defn project-id-for
  "Compute the sidecar project-id for a host-side project-root path.

   The id is the path relative to (workspace-root), with forward slashes —
   matching how analyze.sh resolves $WORKSPACE/$project_id inside the
   container. Returns nil when project-root is outside workspace-root."
  [project-root]
  (when (and project-root (fs/exists? project-root))
    (let [root (fs/canonicalize (workspace-root))
          proj (fs/canonicalize project-root)]
      (cond
        (= proj root)
        ;; Root itself: matches analyze.sh's "workspace" sentinel.
        "workspace"

        (fs/starts-with? proj root)
        (str (fs/relativize root proj))

        :else nil))))

;; =============================================================================
;; Internal Helpers
;; =============================================================================

(defn- read-edn-file
  "Read and parse an EDN file. Returns nil if file missing or parse error."
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (try
        (edn/read-string (slurp f))
        (catch Exception e
          (log/warn "Failed to read EDN file:" path (.getMessage e))
          nil)))))

(defn- cache-path
  "Build path to a cache file for a project."
  [project-id filename]
  (str (cache-dir) "/" project-id "/" filename))

;; In-memory parsed cache. Avoids re-parsing on every call.
;; Keyed by project-id, invalidated when meta.edn timestamp changes.
(defonce ^:private parsed-dump-cache
  (atom {})) ;; {project-id {:timestamp <epoch-s> :data <parsed-map>}}

(defn- pre-extracted-available?
  "Check if pre-extracted focused files exist (produced by sidecar extract.bb)."
  [project-id]
  (let [f (io/file (cache-path project-id "var-defs.edn"))]
    (.exists f)))

(defn- read-pre-extracted
  "Read pre-extracted focused files (var-defs.edn, call-graph.edn, ns-graph.edn).
   These are ~100x smaller than dump.edn and parse in <1 second.
   Returns a synthetic analysis map compatible with the dump format."
  [project-id]
  (let [var-defs   (read-edn-file (cache-path project-id "var-defs.edn"))
        call-graph (read-edn-file (cache-path project-id "call-graph.edn"))
        ns-graph   (read-edn-file (cache-path project-id "ns-graph.edn"))
        ns-defs    (read-edn-file (cache-path project-id "ns-defs.edn"))
        summary    (read-edn-file (cache-path project-id "summary.edn"))]
    (when var-defs
      (log/info "Using pre-extracted files for" project-id
                "(var-defs:" (count var-defs) "calls:" (count call-graph) ")")
      {:pre-extracted true
       :summary       summary
       :var-defs      var-defs
       :call-graph    call-graph
       :dep-graph     ns-graph
       :ns-defs       ns-defs})))

(defn- read-dump-cached
  "Read analysis with in-memory caching. Prefers pre-extracted focused files
   (fast: <1s) and falls back to monolithic dump.edn (slow: 30s+).
   Re-parses only when meta timestamp changes (sidecar re-analyzed)."
  [project-id meta-timestamp]
  (let [cached (get @parsed-dump-cache project-id)]
    (if (and cached (= meta-timestamp (:timestamp cached)))
      (do (log/debug "In-memory cache hit for" project-id)
          (:data cached))
      (let [data (if (pre-extracted-available? project-id)
                   (read-pre-extracted project-id)
                   (do (log/warn "No pre-extracted files, falling back to dump.edn"
                                 "(this may take 30+ seconds for large projects)")
                       (read-edn-file (cache-path project-id "dump.edn"))))]
        (when data
          (log/info "Cached analysis for" project-id
                    (if (:pre-extracted data) "(pre-extracted)" "(monolithic)"))
          (swap! parsed-dump-cache assoc project-id
                 {:timestamp meta-timestamp :data data}))
        data))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn read-meta
  "Read cache metadata for a project.
   Returns map with :timestamp, :status, :duration-ms, etc. or nil."
  [project-id]
  (read-edn-file (cache-path project-id "meta.edn")))

(defn cache-fresh?
  "Check if cached analysis is fresh enough (within max-age-ms).
   Meta timestamp is in epoch-seconds; we compare in milliseconds."
  ([project-id]
   (cache-fresh? project-id default-max-age-ms))
  ([project-id max-age-ms]
   (when-let [meta (read-meta project-id)]
     (and (= :ok (:status meta))
          (let [cache-age-ms (- (System/currentTimeMillis)
                                (* (:timestamp meta) 1000))]
            (< cache-age-ms max-age-ms))))))

(defn read-analysis
  "Read cached analysis dump for a project.

   Returns the full dump result (map with :analysis, :dep-graph, etc.)
   or nil if cache is missing, errored, or stale.

   Options:
     :max-age-ms        - max cache age in ms (default: 10 min)
     :ignore-staleness  - if true, return data even if stale"
  ([project-id]
   (read-analysis project-id {}))
  ([project-id {:keys [max-age-ms ignore-staleness]
                :or   {max-age-ms default-max-age-ms}}]
   (let [meta (read-meta project-id)]
     (cond
       (nil? meta)
       (do (log/info "No cache for project:" project-id)
           nil)

       (not= :ok (:status meta))
       (do (log/warn "Cache error for project:" project-id
                     "status:" (:status meta))
           nil)

       (and (not ignore-staleness)
            (let [age-ms (- (System/currentTimeMillis)
                            (* (:timestamp meta) 1000))]
              (> age-ms max-age-ms)))
       (do (log/warn "Stale cache for project:" project-id
                     "max-age-ms:" max-age-ms)
           nil)

       :else
       (let [dump (read-dump-cached project-id (:timestamp meta))]
         (when dump
           (log/debug "Cache hit for project:" project-id)
           dump))))))

(defn invalidate!
  "Drop the in-memory parsed analysis for `project-id` so the next
   `read-analysis` re-parses from disk. The on-disk dump is untouched.
   Use after a config change forces a fresh sidecar dump."
  [project-id]
  (swap! parsed-dump-cache dissoc project-id)
  nil)

(defn list-cached-projects
  "List all project-ids with cached analysis in the cache directory."
  []
  (let [dir (io/file (cache-dir))]
    (when (.exists dir)
      (->> (.listFiles dir)
           (filter #(.isDirectory ^java.io.File %))
           (filter #(.exists (io/file % "meta.edn")))
           (mapv #(.getName ^java.io.File %))))))

(defn cache-status
  "Return status overview of all cached projects."
  []
  {:cache-dir (cache-dir)
   :projects  (mapv (fn [pid]
                      (let [meta (read-meta pid)]
                        {:project-id  pid
                         :status      (:status meta)
                         :timestamp   (:timestamp meta)
                         :fresh?      (cache-fresh? pid)
                         :duration-ms (:duration-ms meta)}))
                    (list-cached-projects))})

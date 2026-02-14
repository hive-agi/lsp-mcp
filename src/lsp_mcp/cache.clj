(ns lsp-mcp.cache
  "Reads cached clojure-lsp analysis from shared volume.

   The Docker sidecar container periodically runs clojure-lsp dump
   and writes EDN files to a shared cache directory. This namespace
   provides a thin bridge to read that cached data.

   Cache structure:
     <cache-dir>/<project-id>/dump.edn   - full analysis result
     <cache-dir>/<project-id>/meta.edn   - freshness metadata

   CLARITY-L: Pure read-only bridge layer, no domain logic."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [lsp-mcp.log :as log]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:private default-cache-dir
  (str (System/getProperty "user.home") "/.cache/hive-lsp"))
(def ^:private default-max-age-ms
  "Default max age for cached analysis: 10 minutes."
  (* 10 60 1000))

(defn cache-dir
  "Resolve the LSP cache directory.
   Priority: LSP_CACHE_DIR env var > ~/.cache/hive-lsp."
  []
  (or (System/getenv "LSP_CACHE_DIR")
      default-cache-dir))

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

;; In-memory parsed dump cache. Avoids re-parsing 60MB+ EDN on every call.
;; Keyed by project-id, invalidated when meta.edn timestamp changes.
(defonce ^:private parsed-dump-cache
  (atom {})) ;; {project-id {:timestamp <epoch-s> :data <parsed-map>}}

(defn- read-dump-cached
  "Read dump.edn with in-memory caching. Re-parses only when
   meta timestamp changes (sidecar re-analyzed)."
  [project-id meta-timestamp]
  (let [cached (get @parsed-dump-cache project-id)]
    (if (and cached (= meta-timestamp (:timestamp cached)))
      (do (log/debug "In-memory cache hit for" project-id)
          (:data cached))
      (let [path (cache-path project-id "dump.edn")
            data (read-edn-file path)]
        (when data
          (log/info "Parsed dump.edn for" project-id "(caching in memory)")
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

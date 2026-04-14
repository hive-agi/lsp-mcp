(ns lsp-mcp.sidecar
  "On-demand trigger for the Docker LSP sidecar.

   The sidecar (hive-mcp-lsp-sidecar container) runs clojure-lsp dump
   periodically and writes cached analysis to ~/.cache/hive-lsp/<project-id>/.
   It supports on-demand requests via a _request.edn file + SIGHUP.

   This namespace provides:
   - request-analysis!   — write request file + signal sidecar
   - await-cache-ready   — poll cache dir until meta.edn appears
   - ensure-analysis!    — orchestrate: check cache → request → await"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hive-dsl.result :as r]
   [hive-system.shell.core :as sh]
   [lsp-mcp.cache :as cache]
   [lsp-mcp.log :as log]))

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:private sidecar-container
  "Docker container name for the LSP sidecar."
  "hive-mcp-lsp-sidecar")

(def ^:private default-timeout-ms
  "Max time to wait for sidecar analysis to complete."
  60000)

(def ^:private poll-interval-ms
  "Interval between cache-ready polls."
  2000)

;; =============================================================================
;; Sidecar Communication
;; =============================================================================

(defn- request-file-path
  "Path to the sidecar's request file on the shared cache volume."
  []
  (str (cache/cache-dir) "/_request.edn"))

(defn request-analysis!
  "Write project-id to sidecar request file and signal via SIGHUP.
   Returns {:requested true :project-id pid} or {:error str}."
  [project-id]
  (try
    (let [req-file (io/file (request-file-path))]
      ;; Ensure cache dir exists
      (.mkdirs (.getParentFile req-file))
      ;; Append project-id (sidecar reads one per line)
      (spit req-file (str project-id "\n") :append true)
      (log/info "Wrote sidecar request for" project-id)
      ;; Signal sidecar to process immediately (via hive-system IShell)
      (let [result (sh/exec! ["docker" "exec" sidecar-container "kill" "-HUP" "1"])
            {:keys [exit stderr]} (:ok result)]
        (if (and (r/ok? result) (zero? (or exit -1)))
          (do (log/info "Sent SIGHUP to sidecar")
              {:requested true :project-id project-id})
          (do (log/warn "SIGHUP failed (sidecar may not be running):"
                        (or stderr (:err result)))
              ;; Request file still written — sidecar will pick up on next loop
              {:requested true :project-id project-id
               :warning   "SIGHUP failed, sidecar will process on next cycle"}))))
    (catch Exception e
      (log/error "Failed to request sidecar analysis:" (ex-message e))
      {:error (str "sidecar request failed: " (ex-message e))})))

(defn await-cache-ready
  "Poll cache directory until analysis is available for project-id.
   Returns cached analysis map or nil on timeout."
  ([project-id]
   (await-cache-ready project-id default-timeout-ms))
  ([project-id timeout-ms]
   (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
     (loop []
       (when (< (System/currentTimeMillis) deadline)
         (if-let [data (cache/read-analysis project-id {:ignore-staleness true})]
           (do (log/info "Sidecar analysis ready for" project-id)
               data)
           (do (Thread/sleep poll-interval-ms)
               (recur))))))))

;; =============================================================================
;; Orchestrator
;; =============================================================================

(defn ensure-analysis!
  "Ensure analysis is available for project-id.

   Strategy:
   1. Check cache (fast path)
   2. If miss, request sidecar analysis + await
   3. Return analysis map or structured error

   Returns:
     {:ok analysis-map}   on success
     {:error :analysis/sidecar-timeout :fix :trigger-sidecar ...} on timeout
     {:error :analysis/sidecar-unavailable ...} on request failure"
  [project-id]
  ;; Fast path: cache already has it
  (if-let [cached (cache/read-analysis project-id)]
    {:ok cached}
    ;; Slow path: trigger sidecar
    (let [req-result (request-analysis! project-id)]
      (if (:error req-result)
        {:error   :analysis/sidecar-unavailable
         :fix     :start-sidecar
         :command "docker compose up -d lsp-sidecar"
         :hint    "LSP sidecar container not reachable. Start it with docker compose."
         :message (:error req-result)}
        ;; Await cache population
        (if-let [data (await-cache-ready project-id)]
          {:ok data}
          {:error     :analysis/sidecar-timeout
           :fix       :trigger-sidecar
           :command   (str "docker exec " sidecar-container " kill -HUP 1")
           :hint      (str "Sidecar did not produce analysis for '" project-id
                           "' within " (quot default-timeout-ms 1000) "s. "
                           "Check sidecar logs: docker logs " sidecar-container)
           :message   (str "Timed out waiting for sidecar analysis of " project-id)})))))

(defn sidecar-running?
  "Check if the LSP sidecar container is running."
  []
  (try
    (let [result (sh/exec! ["docker" "inspect" "-f" "{{.State.Running}}"
                            sidecar-container])
          {:keys [exit stdout]} (:ok result)]
      (and (r/ok? result)
           (zero? (or exit -1))
           (= "true" (str/trim (or stdout "")))))
    (catch Exception _ false)))

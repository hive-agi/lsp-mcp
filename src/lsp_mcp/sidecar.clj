(ns lsp-mcp.sidecar
  "On-demand trigger for the Docker LSP sidecar.

   The sidecar (hive-mcp-lsp-sidecar container) runs clojure-lsp dump
   periodically and writes cached analysis to ~/.cache/hive-lsp/<project-id>/.
   It supports on-demand requests via a _request.edn file + SIGHUP.

   This namespace provides:
   - ensure-sidecar-running! — readiness check + auto-start container
   - request-analysis!   — write request file + signal sidecar
   - await-cache-ready   — poll cache dir until meta.edn appears
   - ensure-analysis!    — orchestrate: ensure → check cache → request → await"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hive-dsl.result :as r]
   [hive-system.shell.core :as sh]
   [lsp-mcp.cache :as cache]
   [lsp-mcp.log :as log]))

;; Forward decl — sidecar-running? defined at bottom, used by ensure-sidecar-running!
(declare sidecar-running?)

;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:private sidecar-container
  "Docker container name for the LSP sidecar."
  "hive-mcp-lsp-sidecar")

(def ^:private compose-file-candidates
  "Candidate docker-compose paths (first existing wins)."
  ["/home/leibniz/PP/hive/hive-mcp/docker-compose.yml"
   (str (System/getProperty "user.home") "/PP/hive/hive-mcp/docker-compose.yml")])

(def ^:private default-timeout-ms
  "Max time to wait for sidecar analysis to complete.
   Overridable via `LSP_SIDECAR_TIMEOUT_MS` env var — large projects
   (hive-knowledge, hive-mcp with 500+ forms) routinely exceed the
   60s baseline. Invalid values fall back to 60000."
  (or (try
        (some-> (System/getenv "LSP_SIDECAR_TIMEOUT_MS")
                Long/parseLong)
        (catch NumberFormatException _ nil))
      60000))

(def ^:private poll-interval-ms
  "Interval between cache-ready polls."
  2000)

(def ^:private start-timeout-ms
  "Max time to wait for sidecar container to become healthy after start."
  10000)

(def ^:private start-poll-interval-ms
  "Interval between sidecar-running? polls during auto-start."
  500)

;; =============================================================================
;; Sidecar Auto-Start (readiness + lazy spawn)
;; =============================================================================

(defn- find-compose-file
  "Return first existing compose file path, or nil."
  []
  (some (fn [^String p]
          (when (and p (.exists (java.io.File. p))) p))
        compose-file-candidates))

(defn- start-sidecar!
  "Spawn the LSP sidecar container.
   Tries `docker compose up -d lsp-sidecar` if a compose file is found,
   else falls back to `docker start <container>`.
   Returns {:started true} on success or {:error msg}."
  []
  (try
    (let [compose (find-compose-file)
          cmd     (if compose
                    ["docker" "compose" "-f" compose "up" "-d" "lsp-sidecar"]
                    ["docker" "start" sidecar-container])
          _       (log/info "Auto-starting LSP sidecar:" (str/join " " cmd))
          result  (sh/exec! cmd)
          {:keys [exit stderr]} (:ok result)]
      (if (and (r/ok? result) (zero? (or exit -1)))
        (do (log/info "Sidecar start command succeeded")
            {:started true})
        (let [msg (or stderr (:err result) (str "exit=" exit))]
          (log/warn "Sidecar start failed:" msg)
          {:error msg})))
    (catch Exception e
      (log/error "Failed to spawn sidecar:" (ex-message e))
      {:error (ex-message e)})))

(defn- await-sidecar-running
  "Poll sidecar-running? until true or timeout. Returns true/false."
  ([] (await-sidecar-running start-timeout-ms))
  ([timeout-ms]
   (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
     (loop []
       (cond
         (sidecar-running?)                          true
         (>= (System/currentTimeMillis) deadline)    false
         :else                                       (do (Thread/sleep start-poll-interval-ms)
                                                         (recur)))))))

(defn ensure-sidecar-running!
  "Check sidecar readiness; spawn the container on demand if down.

   Returns:
     {:ok true}                          — already running or newly started
     {:error :analysis/sidecar-unavailable
      :fix :start-sidecar
      :command \"docker compose up -d lsp-sidecar\"
      :hint ... :message ...}            — could not start within timeout"
  []
  (if (sidecar-running?)
    {:ok true}
    (let [start-result (start-sidecar!)]
      (if (:error start-result)
        {:error   :analysis/sidecar-unavailable
         :fix     :start-sidecar
         :command "docker compose up -d lsp-sidecar"
         :hint    (str "LSP sidecar container '" sidecar-container
                       "' is not running and auto-start failed. "
                       "Start it manually: docker compose up -d lsp-sidecar")
         :message (:error start-result)}
        (if (await-sidecar-running)
          (do (log/info "Sidecar auto-start completed for" sidecar-container)
              {:ok true})
          {:error   :analysis/sidecar-unavailable
           :fix     :start-sidecar
           :command "docker compose up -d lsp-sidecar"
           :hint    (str "Sidecar container '" sidecar-container
                         "' did not become ready within "
                         (quot start-timeout-ms 1000) "s after start. "
                         "Check: docker logs " sidecar-container)
           :message "sidecar start command issued but container not ready"})))))

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
   2. Auto-start sidecar if not running (lazy spawn)
   3. Request sidecar analysis + await
   4. Return analysis map or structured error

   Returns:
     {:ok analysis-map}   on success
     {:error :analysis/sidecar-timeout :fix :trigger-sidecar ...} on timeout
     {:error :analysis/sidecar-unavailable ...} on request/start failure"
  [project-id]
  ;; Fast path: cache already has it
  (if-let [cached (cache/read-analysis project-id)]
    {:ok cached}
    ;; Slow path: ensure sidecar is up, then trigger
    (let [ready (ensure-sidecar-running!)]
      (if (:error ready)
        ready
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
               :message   (str "Timed out waiting for sidecar analysis of " project-id)})))))))

(defn sidecar-running?
  "Check if the LSP sidecar container is running."
  []
  (r/rescue false
    (let [result (sh/exec! ["docker" "inspect" "-f" "{{.State.Running}}"
                            sidecar-container])
          {:keys [exit stdout]} (:ok result)]
      (and (r/ok? result)
           (zero? (or exit -1))
           (= "true" (str/trim (or stdout "")))))))

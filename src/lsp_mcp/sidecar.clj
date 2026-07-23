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
   [hive-help.diag :as diag]
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
   60s baseline. First-time scans of unfamiliar projects also need
   extra headroom: clojure-lsp downloads transitive deps (mvn + git
   pulls for :git/url libs), and shadow-cljs / lein classpath probes
   add minutes on top of analysis. 5 minutes is the new default — long
   enough for a cold-start payment-write-service with private git deps,
   short enough that a wedged container still surfaces as an error.
   Invalid values fall back to 300000."
  (or (try
        (some-> (System/getenv "LSP_SIDECAR_TIMEOUT_MS")
                Long/parseLong)
        (catch NumberFormatException _ nil))
      300000))

(def ^:private poll-interval-ms
  "Interval between cache-ready polls."
  2000)

(def ^:private start-timeout-ms
  "Max time to wait for sidecar container to become healthy after start."
  10000)

(def ^:private start-poll-interval-ms
  "Interval between sidecar-running? polls during auto-start."
  500)

(def ^:private heartbeat-timeout-ms
  "A running container without a recent worker heartbeat is degraded."
  (or (try
        (some-> (System/getenv "LSP_SIDECAR_HEARTBEAT_TIMEOUT_MS")
                Long/parseLong)
        (catch NumberFormatException _ nil))
      30000))

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

(defn- job-file-path
  [project-id]
  (str (cache/cache-dir) "/" project-id "/job.edn"))

(defn- cancel-file-path
  [project-id]
  (str (cache/cache-dir) "/" project-id "/cancel.edn"))

(defn- atomic-write-edn!
  "Replace an EDN state file atomically so readers never observe partial data."
  [path value]
  (let [target (.toPath (io/file path))
        parent (.getParent target)]
    (java.nio.file.Files/createDirectories
     parent
     (make-array java.nio.file.attribute.FileAttribute 0))
    (let [temp (java.nio.file.Files/createTempFile
                parent
                ".lsp-mcp-"
                ".tmp"
                (make-array java.nio.file.attribute.FileAttribute 0))]
      (try
        (spit (.toFile temp) (pr-str value))
        (try
          (java.nio.file.Files/move
           temp target
           (into-array
            java.nio.file.CopyOption
            [java.nio.file.StandardCopyOption/ATOMIC_MOVE
             java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
          (catch java.nio.file.AtomicMoveNotSupportedException _
            (java.nio.file.Files/move
             temp target
             (into-array
              java.nio.file.CopyOption
              [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))))
        value
        (finally
          (java.nio.file.Files/deleteIfExists temp))))))

(defonce ^:private request-lock
  (Object.))

(defn request-analysis!
  "Queue a sidecar analysis and persist its observable lifecycle."
  [project-id]
  (try
    (let [request-file (io/file (request-file-path))
          job-id       (str (java.util.UUID/randomUUID))
          queued-at-ms (System/currentTimeMillis)
          job           {:job-id job-id
                         :project-id project-id
                         :status :queued
                         :queued-at-ms queued-at-ms}]
      (.mkdirs (.getParentFile request-file))
      (java.nio.file.Files/deleteIfExists
       (.toPath (io/file (cancel-file-path project-id))))
      (atomic-write-edn! (job-file-path project-id) job)
      (locking request-lock
        (spit request-file (str project-id "\n") :append true))
      (log/info "Queued sidecar job" job-id "for" project-id)
      (let [result (sh/exec! ["docker" "exec" sidecar-container
                              "kill" "-HUP" "1"])
            {:keys [exit stderr]} (:ok result)
            response (assoc job :requested true)]
        (if (and (r/ok? result) (zero? (or exit -1)))
          (do
            (log/info "Sent SIGHUP to sidecar")
            response)
          (do
            (log/warn "SIGHUP failed (sidecar will poll the queue):"
                      (or stderr (:err result)))
            (assoc response
                   :warning "SIGHUP failed, sidecar will process on next cycle")))))
    (catch Exception e
      (log/error "Failed to request sidecar analysis:" (ex-message e))
      {:error (str "sidecar request failed: " (ex-message e))})))

(def ^:private terminal-job-statuses
  #{:ok :error :cancelled})

(defn job-status
  "Return the latest persisted job, optionally requiring a matching job-id."
  ([project-id]
   (job-status project-id nil))
  ([project-id expected-job-id]
   (let [job (cache/read-job project-id)]
     (cond
       (nil? job)
       {:error :analysis/job-not-found
        :project-id project-id
        :message (str "No sidecar job exists for " project-id)}

       (and expected-job-id
            (not= expected-job-id (:job-id job)))
       {:error :analysis/job-mismatch
        :project-id project-id
        :job-id expected-job-id
        :current-job-id (:job-id job)
        :message "Requested job-id is no longer current"}

       :else job))))

(defn cancel-analysis!
  "Request cancellation for the current queued or running sidecar job."
  ([project-id]
   (cancel-analysis! project-id nil))
  ([project-id expected-job-id]
   (let [job (cache/read-job project-id)]
     (cond
       (nil? job)
       {:error :analysis/job-not-found
        :project-id project-id
        :message (str "No sidecar job exists for " project-id)}

       (and expected-job-id
            (not= expected-job-id (:job-id job)))
       {:error :analysis/job-mismatch
        :project-id project-id
        :job-id expected-job-id
        :current-job-id (:job-id job)
        :message "Refusing to cancel a superseded job"}

       (terminal-job-statuses (:status job))
       {:error :analysis/job-terminal
        :project-id project-id
        :job-id (:job-id job)
        :status (:status job)
        :message "Sidecar job is already terminal"}

       :else
       (let [requested-at-ms (System/currentTimeMillis)
             marker {:job-id (:job-id job)
                     :project-id project-id
                     :requested-at-ms requested-at-ms}
             updated (assoc job
                            :status :cancelling
                            :cancel-requested-at-ms requested-at-ms)
             _ (atomic-write-edn! (job-file-path project-id) updated)
             _ (atomic-write-edn! (cancel-file-path project-id) marker)
             worker-pid (:worker-pid job)
             kill-result
             (when worker-pid
               (sh/exec! ["docker" "exec" sidecar-container
                          "kill" "-TERM" (str worker-pid)]))
             kill-failed?
             (and kill-result
                  (or (not (r/ok? kill-result))
                      (not (zero? (or (get-in kill-result [:ok :exit])
                                      -1)))))]
         (cond-> (assoc updated :cancel-requested true)
           kill-failed?
           (assoc :warning
                  "Cancellation marker written; worker signal failed")))))))

(defn- analysis-generation
  "Return the sidecar generation token carried by meta.edn."
  [meta]
  (when meta
    (select-keys meta
                 [:job-id :completed-at-ms :timestamp :status
                  :duration-ms :exit-code])))

(defn- dump-failed-result
  "Translate terminal sidecar metadata into an actionable Result error."
  [project-id meta]
  (let [log-path (str (cache/cache-dir) "/" project-id "/dump.log")]
    {:error      :analysis/sidecar-dump-failed
     :fix        :inspect-sidecar-log
     :project-id project-id
     :exit-code  (:exit-code meta)
     :log-path   log-path
     :command    (str "sed -n '1,240p' " log-path)
     :hint       (str "clojure-lsp failed for '" project-id
                      "'. Inspect its dump log; classpath or dependency resolution "
                      "is the usual cause.")
     :message    (str "Sidecar analysis failed for " project-id
                      " with exit code " (:exit-code meta))}))

(defn- cancelled-result
  [project-id meta]
  {:error :analysis/sidecar-cancelled
   :project-id project-id
   :job-id (:job-id meta)
   :status :cancelled
   :message (str "Sidecar analysis was cancelled for " project-id)})

(defn await-cache-ready
  "Wait for a sidecar generation newer than baseline-generation.

   Returns {:ok analysis} for a completed dump, a structured terminal error,
   or nil when no terminal generation arrives before timeout."
  ([project-id]
   (await-cache-ready project-id nil default-timeout-ms))
  ([project-id timeout-ms]
   (await-cache-ready project-id nil timeout-ms))
  ([project-id baseline-generation timeout-ms]
   (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
     (loop []
       (if (>= (System/currentTimeMillis) deadline)
         nil
         (let [meta       (cache/read-meta project-id)
               generation (analysis-generation meta)]
           (cond
             (= generation baseline-generation)
             (do
               (Thread/sleep poll-interval-ms)
               (recur))

             (= :cancelled (:status meta))
             (cancelled-result project-id meta)

             (= :error (:status meta))
             (dump-failed-result project-id meta)

             (= :ok (:status meta))
             (do
               (cache/invalidate! project-id)
               (if-let [data (cache/read-analysis
                              project-id
                              {:ignore-staleness true})]
                 (do
                   (log/info "Sidecar analysis ready for" project-id)
                   {:ok data})
                 {:error :analysis/sidecar-cache-invalid
                  :fix :inspect-sidecar-log
                  :project-id project-id
                  :message (str "Sidecar marked analysis successful for "
                                project-id
                                " but no readable dump exists")}))

             :else
             (do
               (Thread/sleep poll-interval-ms)
               (recur)))))))))

;; =============================================================================
;; Orchestrator
;; =============================================================================

(defn- project-id-resolves?
  "True when `project-id` maps to a host directory the sidecar can see.

   The sidecar mounts `(cache/workspace-root)` as `/workspace` and
   resolves a request as `/workspace/<project-id>`. If that host path
   does not exist as a directory, awaiting the cache would burn the
   full timeout while clojure-lsp dump fails inside the container.

   Special case: `\"workspace\"` is the sentinel for the workspace
   root itself (matches `cache/project-id-for` behavior)."
  [project-id]
  (or (= "workspace" project-id)
      (let [^java.io.File f (java.io.File.
                             ^String (str (cache/workspace-root) "/" project-id))]
        (.isDirectory f))))

(defn- unresolvable-project-id-error
  "Build the structured error for a project-id that doesn't map to a
   host directory under the workspace mount. Uses hive-help.diag for
   the ELM-shaped message."
  [project-id]
  (let [ws (cache/workspace-root)]
    {:error   :analysis/unresolvable-project-id
     :fix     :pass-resolvable-id
     :command "ls $HIVE_LSP_WORKSPACE_ROOT"
     :project-id project-id
     :workspace-root (str ws)
     :hint    (diag/unresolvable-scope-message
               {:scope    project-id
                :tried    [{:strategy "host directory under workspace mount"
                            :result   (str "no directory at " ws "/" project-id)}]
                :examples [(str ws "/<some-existing-subdir>")
                           "workspace  ; sentinel for the mount itself"]
                :hint     (str "The sidecar resolves project-id as <workspace>/<id>. "
                               "Pass an existing host directory under "
                               (pr-str (str ws)) ", or the literal "
                               "\"workspace\" for the mount itself.")})
     :message (str "Cannot resolve project-id " (pr-str project-id)
                   " to a host directory under workspace " (pr-str (str ws)))}))

(defn ensure-analysis!
  "Ensure analysis is available for project-id.

   Fresh cache hits return immediately. Cache misses request a new sidecar
   generation and distinguish terminal dump failures from genuine timeouts."
  [project-id]
  (if-let [cached (cache/read-analysis project-id)]
    {:ok cached}
    (if-not (project-id-resolves? project-id)
      (unresolvable-project-id-error project-id)
      (let [ready (ensure-sidecar-running!)]
        (if (:error ready)
          ready
          (let [baseline   (analysis-generation (cache/read-meta project-id))
                req-result (request-analysis! project-id)]
            (if (:error req-result)
              {:error   :analysis/sidecar-unavailable
               :fix     :start-sidecar
               :command "docker compose up -d lsp-sidecar"
               :hint    "LSP sidecar container not reachable. Start it with docker compose."
               :message (:error req-result)}
              (or (await-cache-ready project-id baseline default-timeout-ms)
                  {:error   :analysis/sidecar-timeout
                   :fix     :trigger-sidecar
                   :command (str "docker exec " sidecar-container " kill -HUP 1")
                   :hint    (str "Sidecar produced no terminal generation for '"
                                 project-id "' within "
                                 (quot default-timeout-ms 1000) "s. "
                                 "Check sidecar logs: docker logs " sidecar-container)
                   :message (str "Timed out waiting for sidecar analysis of "
                                 project-id)}))))))))

(defn refresh-analysis!
  "Force a fresh sidecar generation for project-id.

   Waits for a generation token change, returns terminal dump failures
   immediately, and invalidates the parsed cache before reading the new dump."
  [project-id]
  (if-not (project-id-resolves? project-id)
    (unresolvable-project-id-error project-id)
    (let [old-meta (cache/read-meta project-id)
          baseline (analysis-generation old-meta)
          ready    (ensure-sidecar-running!)]
      (if (:error ready)
        ready
        (let [req (request-analysis! project-id)]
          (if (:error req)
            req
            (if-let [outcome (await-cache-ready project-id baseline default-timeout-ms)]
              (if (:ok outcome)
                (let [new-meta (cache/read-meta project-id)]
                  (log/info "Sidecar analysis refreshed for" project-id
                            "old-ts:" (:timestamp old-meta)
                            "new-ts:" (:timestamp new-meta))
                  {:refreshed? true
                   :old-ts     (:timestamp old-meta)
                   :new-ts     (:timestamp new-meta)})
                outcome)
              {:error   :analysis/sidecar-timeout
               :fix     :trigger-sidecar
               :command (str "docker exec " sidecar-container " kill -HUP 1")
               :hint    (str "Sidecar produced no fresh terminal generation for '"
                             project-id "' within "
                             (quot default-timeout-ms 1000) "s. "
                             "Check sidecar logs: docker logs " sidecar-container)
               :old-ts  (:timestamp old-meta)
               :message (str "Timed out waiting for fresh sidecar analysis of "
                             project-id)})))))))

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

(defn health
  "Report functional worker health, not merely container process state."
  []
  (let [container-running? (sidecar-running?)
        heartbeat          (cache/read-sidecar-health)
        heartbeat-at-ms    (:heartbeat-at-ms heartbeat)
        heartbeat-age-ms   (when (number? heartbeat-at-ms)
                             (max 0 (- (System/currentTimeMillis)
                                       heartbeat-at-ms)))
        heartbeat-fresh?   (and heartbeat-age-ms
                                (<= heartbeat-age-ms
                                    heartbeat-timeout-ms))
        inventory          (cache/cache-status)
        status             (cond
                             (not container-running?) :down
                             (not heartbeat-fresh?) :degraded
                             (not= :ok (:status heartbeat)) :degraded
                             :else :ok)]
    {:status status
     :functional? (= :ok status)
     :container-running? container-running?
     :heartbeat heartbeat
     :heartbeat-age-ms heartbeat-age-ms
     :heartbeat-timeout-ms heartbeat-timeout-ms
     :queue (:queue inventory)
     :cached-projects (count (:projects inventory))}))

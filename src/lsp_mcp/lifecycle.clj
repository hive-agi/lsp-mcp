(ns lsp-mcp.lifecycle
  "Manual stop utility for the clojure-lsp sidecar container.

   The sidecar (`hive-mcp-lsp-sidecar`) is a long-lived Docker service
   that intentionally outlives hive-mcp JVM restarts — keeping the
   clojure-lsp analysis cache warm avoids cold re-index costs. It is
   NOT wired into shutdown orchestration; there is no IShutdownHook.

   `stop-sidecar!` is exposed for operator maintenance (container
   upgrade, host reboot preparation). `docker stop -t 3 <name>` maps
   to SIGTERM -> 3 s grace -> SIGKILL escalation natively."
  ;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
  ;;
  ;; SPDX-License-Identifier: AGPL-3.0-or-later
  (:require [clojure.string :as str]
            [hive-system.shell.core :as sh]
            [hive-dsl.result :as r]
            [lsp-mcp.log :as log]))

;; =============================================================================
;; Sidecar identity
;; =============================================================================

(def ^:private sidecar-container
  "Docker container name for the LSP sidecar. Kept in sync with
   lsp-mcp.sidecar/sidecar-container (duplicated to avoid a cyclic
   require between the two namespaces during shutdown)."
  "hive-mcp-lsp-sidecar")

(def ^:private grace-period-seconds
  "Seconds Docker waits after SIGTERM before escalating to SIGKILL."
  3)

;; =============================================================================
;; Stop logic — idempotent, never throws
;; =============================================================================

(defn- sidecar-running?
  "Return true if the sidecar container is currently running.
   Returns false on any failure (docker missing, container absent, etc.)."
  []
  (r/rescue false
    (let [result (sh/exec! ["docker" "inspect" "-f" "{{.State.Running}}"
                            sidecar-container])
          {:keys [exit stdout]} (:ok result)]
      (and (r/ok? result)
           (zero? (or exit -1))
           (= "true" (str/trim (or stdout "")))))))

(defn stop-sidecar!
  "Stop the sidecar container with `docker stop -t <grace>`.

   Idempotent: no-op when the container is absent or already stopped.
   Never throws — catches Throwable and logs.

   SIGTERM -> `grace-period-seconds` -> SIGKILL is implemented by
   Docker itself; we only pass `-t`."
  [& _]
  (try
    (if-not (sidecar-running?)
      (log/debug "LSP sidecar not running; stop is a no-op")
      (let [cmd    ["docker" "stop" "-t" (str grace-period-seconds)
                    sidecar-container]
            _      (log/info "Stopping LSP sidecar:" sidecar-container
                             "(grace" grace-period-seconds "s)")
            result (sh/exec! cmd)
            {:keys [exit stderr]} (:ok result)]
        (if (and (r/ok? result) (zero? (or exit -1)))
          (log/info "LSP sidecar stopped cleanly")
          (log/warn "LSP sidecar stop returned non-zero:"
                    (or stderr (:err result) (str "exit=" exit))))))
    (catch Throwable t
      (log/error "LSP sidecar stop failed:" (.getMessage t)))))


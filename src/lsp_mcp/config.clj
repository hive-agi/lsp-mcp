(ns lsp-mcp.config
  "Typed config for lsp-mcp via hive-di.

   Centralizes env-var resolution so the ad-hoc `(or (System/getenv X) D)`
   pattern does not spread across cache.clj and sidecar.clj.

   Env vars
   --------
     LSP_CACHE_DIR            → :cache-dir       (default: ~/.cache/hive-lsp)
     HIVE_LSP_WORKSPACE_ROOT  → :workspace-root  (default: ~/PP)

   Accessors
   ---------
   `(cache-dir)` and `(workspace-root)` read from a lazily resolved,
   atom-cached config map. Tests can force re-resolution via `(reload!)`.

   Follows the hive-di defconfig pattern — see
   hive-knowledge.carto-editing.config and hive-agent.config for
   larger exemplars."
  (:require [hive-di.core :refer [defconfig env]]))

;; =============================================================================
;; Defaults — eagerly resolved at ns load (matches legacy behavior)
;; =============================================================================

(def ^:private default-cache-dir
  (str (System/getProperty "user.home") "/.cache/hive-lsp"))

(def ^:private default-workspace-root
  (str (System/getProperty "user.home") "/PP"))

;; =============================================================================
;; LspConfig — typed, ADT-backed
;; =============================================================================

(defconfig LspConfig
  :cache-dir      (env "LSP_CACHE_DIR"
                       :default default-cache-dir
                       :type :string
                       :doc "Host-side directory the sidecar writes analysis EDN files to.")
  :workspace-root (env "HIVE_LSP_WORKSPACE_ROOT"
                       :default default-workspace-root
                       :type :string
                       :doc "Host-side root that the sidecar container mounts as /workspace."))

;; =============================================================================
;; Resolved-config cache (lazy, reloadable)
;; =============================================================================

(def ^:private fallback-config
  "Used when hive-di resolution fails (shouldn't happen — both fields
   have defaults — but keeps accessors total)."
  {:cache-dir      default-cache-dir
   :workspace-root default-workspace-root})

(defn- resolve-cached []
  (let [r (resolve-LspConfig)]
    (or (:ok r) fallback-config)))

(def ^:private config-cache (atom (delay (resolve-cached))))

(defn reload!
  "Force re-resolution of LspConfig against current env.
   Returns the freshly resolved map. REPL / test use."
  []
  (reset! config-cache (delay (resolve-cached)))
  @@config-cache)

(defn current
  "Return the currently cached resolved config map."
  []
  @@config-cache)

;; =============================================================================
;; Field accessors
;; =============================================================================

(defn cache-dir
  "Resolve the LSP cache directory.
   Priority: LSP_CACHE_DIR env var > ~/.cache/hive-lsp."
  []
  (:cache-dir (current)))

(defn workspace-root
  "Host-side root mounted by the sidecar as /workspace.
   Priority: HIVE_LSP_WORKSPACE_ROOT env var > ~/PP."
  []
  (:workspace-root (current)))

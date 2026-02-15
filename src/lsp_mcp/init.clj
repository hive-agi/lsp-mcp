(ns lsp-mcp.init
  "IAddon implementation for lsp-mcp — Clojure LSP analysis & KG sync.

   Deterministic L1 structural layer. Contributes the 'lsp' MCP tool.
   Follows the hive-knowledge exemplar: reify + nil-railway pipeline.

   Usage:
     ;; Via addon system (auto-discovered from META-INF manifest):
     (init-as-addon!)

     ;; Legacy fallback:
     (register-tools!)"
  (:require [lsp-mcp.tools :as tools]
            [lsp-mcp.cache :as cache]
            [lsp-mcp.log :as log]))

;; =============================================================================
;; Resolution Helpers
;; =============================================================================

(defn- try-resolve
  "Attempt to resolve a fully-qualified symbol. Returns var or nil."
  [sym]
  (try
    (requiring-resolve sym)
    (catch Exception _ nil)))

;; =============================================================================
;; IAddon Implementation
;; =============================================================================

(defonce ^:private addon-instance (atom nil))

(defn- make-addon
  "Create an IAddon reify for lsp-mcp.
   Returns nil if protocol is not on classpath."
  []
  (when (try-resolve 'hive-mcp.addons.protocol/IAddon)
    (let [state (atom {:initialized? false})]
      (reify
        hive-mcp.addons.protocol/IAddon

        (addon-id [_] "lsp.mcp")

        (addon-type [_] :native)

        (capabilities [_] #{:tools})

        (initialize! [_ _config]
          (if (:initialized? @state)
            {:success? true :already-initialized? true}
            (do
              (reset! state {:initialized? true})
              ;; Contribute commands to composite "analysis" tool
              (when-let [contribute! (try-resolve 'hive-mcp.extensions.registry/contribute-commands!)]
                (contribute! "analysis" :lsp
                             {"definitions" {:handler #(tools/handle-lsp (assoc % :command "definitions"))
                                             :params {"project_root" {:type "string" :description "Path to the project root directory"}
                                                      "namespace" {:type "string" :description "Filter by namespace (e.g., my.app.core)"}}
                                             :description "List var definitions in project/namespace"}
                              "references"  {:handler #(tools/handle-lsp (assoc % :command "references"))
                                             :params {"project_root" {:type "string" :description "Path to the project root directory"}
                                                      "function" {:type "string" :description "Filter by function name"}
                                                      "namespace" {:type "string" :description "Filter by namespace"}}
                                             :description "Find references to a function"}
                              "ns-graph"    {:handler #(tools/handle-lsp (assoc % :command "ns-graph"))
                                             :params {"project_root" {:type "string" :description "Path to the project root directory"}}
                                             :description "Namespace dependency graph (LSP-based)"}
                              "sync"        {:handler #(tools/handle-lsp (assoc % :command "sync"))
                                             :params {"project_root" {:type "string" :description "Path to the project root directory"}
                                                      "project_id" {:type "string" :description "Project identifier for KG sync"}
                                                      "scope" {:type "string" :description "Scope for KG sync operations"}}
                                             :description "Sync analysis results to Knowledge Graph"}
                              "status"      {:handler #(tools/handle-lsp (assoc % :command "status"))
                                             :params {}
                                             :description "Check LSP bridge and cache status"}
                              ;; Live LSP bridge commands (backend-agnostic)
                              "hover"       {:handler #(tools/handle-lsp (assoc % :command "hover"))
                                             :params {"project_root" {:type "string" :description "Path to the project root directory"}
                                                      "file_path" {:type "string" :description "Path to file"}
                                                      "line" {:type "integer" :description "0-based line number"}
                                                      "column" {:type "integer" :description "0-based column number"}}
                                             :description "Hover info at position (docstring, type)"}
                              "definition"  {:handler #(tools/handle-lsp (assoc % :command "definition"))
                                             :params {"project_root" {:type "string" :description "Path to the project root directory"}
                                                      "file_path" {:type "string" :description "Path to file"}
                                                      "line" {:type "integer" :description "0-based line number"}
                                                      "column" {:type "integer" :description "0-based column number"}}
                                             :description "Go to definition at position"}
                              "live-references" {:handler #(tools/handle-lsp (assoc % :command "live-references"))
                                                 :params {"project_root" {:type "string" :description "Path to the project root directory"}
                                                          "file_path" {:type "string" :description "Path to file"}
                                                          "line" {:type "integer" :description "0-based line number"}
                                                          "column" {:type "integer" :description "0-based column number"}}
                                                 :description "Find all references at position (live)"}
                              "symbols"     {:handler #(tools/handle-lsp (assoc % :command "symbols"))
                                             :params {"project_root" {:type "string" :description "Path to the project root directory"}
                                                      "file_path" {:type "string" :description "Path to file"}}
                                             :description "Document symbols for file"}
                              "cursor-info" {:handler #(tools/handle-lsp (assoc % :command "cursor-info"))
                                             :params {"project_root" {:type "string" :description "Path to the project root directory"}
                                                      "file_path" {:type "string" :description "Path to file"}
                                                      "line" {:type "integer" :description "0-based line number"}
                                                      "column" {:type "integer" :description "0-based column number"}}
                                             :description "Cursor info at position (clojure-lsp cursorInfo)"}
                              "server-info" {:handler #(tools/handle-lsp (assoc % :command "server-info"))
                                             :params {"project_root" {:type "string" :description "Path to the project root directory"}}
                                             :description "LSP server info for project"}
                              "workspaces"  {:handler #(tools/handle-lsp (assoc % :command "workspaces"))
                                             :params {}
                                             :description "List active LSP workspaces"}
                              "bridge-status" {:handler #(tools/handle-lsp (assoc % :command "bridge-status"))
                                               :params {}
                                               :description "Check live LSP bridge availability"}}))
              (log/info "lsp-mcp addon initialized")
              {:success? true
               :errors []
               :metadata {:tools 0
                          :cache-status (cache/cache-status)}})))

        (shutdown! [_]
          (when (:initialized? @state)
            (tools/invalidate-cache!)
            (reset! state {:initialized? false}))
          nil)

        (tools [_]
          ;; Commands contributed to composite "analysis" tool, no standalone tool
          [])

        (schema-extensions [_] {})

        (health [_]
          (if (:initialized? @state)
            {:status  :ok
             :details {:cache (cache/cache-status)}}
            {:status  :down
             :details {:reason "not initialized"}}))))))

;; =============================================================================
;; Dep Registry + Nil-Railway Pipeline
;; =============================================================================

(defonce ^:private dep-registry
  (atom {:register! 'hive-mcp.addons.core/register-addon!
         :init!     'hive-mcp.addons.core/init-addon!
         :addon-id  'hive-mcp.addons.protocol/addon-id}))

(defn- resolve-deps
  "Resolve all symbols in registry. Returns ctx map or nil."
  [registry]
  (reduce-kv
   (fn [ctx k sym]
     (if-let [resolved (try-resolve sym)]
       (assoc ctx k resolved)
       (do (log/debug "Dep resolution failed:" k "->" sym)
           (reduced nil))))
   {}
   registry))

(defn- step-resolve-deps [ctx]
  (when-let [deps (resolve-deps @dep-registry)]
    (merge ctx deps)))

(defn- step-register [{:keys [addon register!] :as ctx}]
  (let [result (register! addon)]
    (when (:success? result)
      (assoc ctx :reg-result result))))

(defn- step-init [{:keys [addon addon-id init!] :as ctx}]
  (let [result (init! (addon-id addon))]
    (when (:success? result)
      (assoc ctx :init-result result))))

(defn- step-store-instance [{:keys [addon] :as ctx}]
  (reset! addon-instance addon)
  ctx)

(defn- run-addon-pipeline!
  "Nil-railway: resolve-deps -> register -> init -> store"
  [initial-ctx]
  (some-> initial-ctx
          step-resolve-deps
          step-register
          step-init
          step-store-instance))

;; =============================================================================
;; Public API
;; =============================================================================

(defn register-tools!
  "Legacy tool registration (pre-IAddon). Returns tool-def seq."
  []
  [(tools/tool-def)])

(defn init-as-addon!
  "Register lsp-mcp as an IAddon. Falls back to legacy register-tools!."
  []
  (if-let [result (some-> (make-addon)
                          (as-> addon (run-addon-pipeline! {:addon addon})))]
    (do
      (log/info "lsp-mcp registered as IAddon")
      {:registered ["lsp"] :total 1})
    (do
      (log/debug "IAddon unavailable, falling back to legacy init")
      {:registered (mapv :name (register-tools!)) :total 1})))

(defn available?
  "Check if LSP analysis backend is reachable (sidecar cache or clojure-lsp)."
  []
  (or (seq (cache/cache-status))
      (try
        (requiring-resolve 'clojure-lsp.api/analyze-project-and-deps!)
        true
        (catch Exception _ false))))

(defn get-addon-instance
  "Return the current IAddon instance, or nil."
  []
  @addon-instance)

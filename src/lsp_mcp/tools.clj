(ns lsp-mcp.tools
  "MCP tool handlers for LSP analysis.

   Three strategies:
   1. Docker sidecar cache (via lsp-mcp.cache) — fast reads
   2. In-process clojure-lsp — fallback when cache unavailable
   3. Live Emacs LSP bridge (via lsp-mcp.emacs-bridge) — real-time queries

   Request-level memoization: multiple commands in quick succession
   (e.g., definitions then calls) share the same analysis result
   via a 30-second TTL cache keyed by project_root."
  (:require [lsp-mcp.bridge :as bridge]
            [lsp-mcp.core :as core]
            [lsp-mcp.cache :as cache]
            [lsp-mcp.analysis :as analysis]
            [lsp-mcp.log :as log]
            [clojure.string :as str]))

;; =============================================================================
;; Request-Level Memoization (30s TTL)
;; =============================================================================

(def ^:private analysis-cache
  "Atom holding {:project-root str :result map :timestamp-ms long}.
   Single-entry cache — only the most recent project is cached."
  (atom nil))

(def ^:private cache-ttl-ms
  "TTL for memoized analysis results: 30 seconds."
  30000)

(defn- cached-analyze
  "Analyze with 30s TTL memoization. Returns raw analysis result map.
   Cache is keyed by project-root; stale or mismatched entries are evicted."
  [project-root]
  (let [now    (System/currentTimeMillis)
        cached @analysis-cache]
    (if (and cached
             (= project-root (:project-root cached))
             (< (- now (:timestamp-ms cached)) cache-ttl-ms))
      (do
        (log/debug "Analysis cache hit for" project-root)
        (:result cached))
      (let [result (core/analyze project-root)]
        (log/debug "Analysis cache miss for" project-root ", caching result")
        (reset! analysis-cache {:project-root project-root
                                :result       result
                                :timestamp-ms now})
        result))))

(defn invalidate-cache!
  "Clear the analysis cache. Useful for testing or after known mutations."
  []
  (reset! analysis-cache nil))

;; =============================================================================
;; Bridge Resolution (lazy — Emacs backend is optional)
;; =============================================================================

(defonce ^:private bridge-instance
  (delay
    (try
      (when-let [make-fn (requiring-resolve 'lsp-mcp.emacs-bridge/make-emacs-bridge)]
        (let [b (make-fn)]
          (when (bridge/bridge-available? b)
            (log/info "Emacs LSP bridge available")
            b)))
      (catch Exception e
        (log/debug "Emacs bridge not available:" (.getMessage e))
        nil))))

(defn- resolve-bridge
  "Get the active ILspBridge instance, or return {:error ...}."
  []
  (or @bridge-instance
      {:error "Emacs LSP bridge not available"}))

;; =============================================================================
;; Command Handlers
;; =============================================================================

(defn- require-project-root
  "Validate project_root param. Returns nil if valid, error response if invalid."
  [params]
  (when (str/blank? (:project_root params))
    {:content [{:type "text" :text (pr-str {:error   "project_root is required"
                                            :command (:command params)})}]
     :isError true}))

(def ^:private command-handlers
  {"analyze"     (fn [{:keys [project_root] :as params}]
                   (or (require-project-root params)
                       (let [result   (cached-analyze project_root)
                             vars     (analysis/extract-var-definitions (:analysis result))
                             ns-graph (analysis/extract-namespace-graph (:dep-graph result))]
                         {:num-files      (count (:analysis result))
                          :num-namespaces (count ns-graph)
                          :num-vars       (count vars)
                          :cache-status   (cache/cache-status)})))
   "definitions" (fn [{:keys [project_root] :as params}]
                   (or (require-project-root params)
                       (let [result (cached-analyze project_root)
                             vars   (analysis/extract-var-definitions (:analysis result))]
                         (if (:namespace params)
                           (vec (filter #(= (str (:ns %)) (:namespace params)) vars))
                           vars))))
   "calls"       (fn [{:keys [project_root namespace function] :as params}]
                   (or (require-project-root params)
                       (let [result (cached-analyze project_root)
                             calls  (analysis/extract-call-graph (:analysis result))]
                         (cond->> calls
                           namespace (filter #(= (str (:caller-ns %)) namespace))
                           function  (filter #(= (str (:caller-fn %)) function))
                           true      vec))))
   "ns-graph"    (fn [{:keys [project_root] :as params}]
                   (or (require-project-root params)
                       (let [result (cached-analyze project_root)]
                         (analysis/extract-namespace-graph (:dep-graph result)))))
   "sync"        (fn [{:keys [project_root project_id scope] :as params}]
                   (or (require-project-root params)
                       (core/analyze-and-sync! project_root project_id scope)))
   "status"      (fn [_]
                   {:bridge-available? (some? (try (requiring-resolve 'hive-mcp.knowledge-graph.edges/add-edge!)
                                                   (catch Exception _ nil)))
                    :cache             (cache/cache-status)})
   "callers"     (fn [{:keys [project_root function namespace] :as params}]
                   (or (require-project-root params)
                       (let [result (cached-analyze project_root)
                             calls  (analysis/extract-call-graph (:analysis result))]
                         (->> calls
                              (filter (fn [c]
                                        (and (or (nil? function)  (= (str (:callee-fn c)) function))
                                             (or (nil? namespace) (= (str (:callee-ns c)) namespace)))))
                              vec))))
   "references"  (fn [{:keys [project_root function namespace] :as params}]
                   (or (require-project-root params)
                       (let [result (cached-analyze project_root)
                             calls  (analysis/extract-call-graph (:analysis result))]
                         (->> calls
                              (filter (fn [c]
                                        (and (or (nil? function)  (= (str (:callee-fn c)) function))
                                             (or (nil? namespace) (= (str (:callee-ns c)) namespace)))))
                              (mapv (fn [c]
                                      {:file      (:file c)
                                       :row       (:row c)
                                       :caller-ns (:caller-ns c)
                                       :caller-fn (:caller-fn c)}))))))
   ;; Strategy 3: Live LSP bridge commands (via ILspBridge protocol — backend-agnostic)
   "bridge-status"     (fn [_]
                         (when-let [b (resolve-bridge)]
                           (bridge/bridge-status b)))
   "workspaces"        (fn [_]
                         (when-let [b (resolve-bridge)]
                           (bridge/bridge-workspaces b)))
   "hover"             (fn [{:keys [project_root file_path line column]}]
                         (when-let [b (resolve-bridge)]
                           (bridge/bridge-hover b project_root file_path line column)))
   "definition"        (fn [{:keys [project_root file_path line column]}]
                         (when-let [b (resolve-bridge)]
                           (bridge/bridge-definition b project_root file_path line column)))
   "live-references"   (fn [{:keys [project_root file_path line column]}]
                         (when-let [b (resolve-bridge)]
                           (bridge/bridge-references b project_root file_path line column)))
   "symbols"           (fn [{:keys [project_root file_path]}]
                         (when-let [b (resolve-bridge)]
                           (bridge/bridge-document-symbols b project_root file_path)))
   "cursor-info"       (fn [{:keys [project_root file_path line column]}]
                         (when-let [b (resolve-bridge)]
                           (bridge/bridge-cursor-info b project_root file_path line column)))
   "server-info"       (fn [{:keys [project_root]}]
                         (when-let [b (resolve-bridge)]
                           (bridge/bridge-server-info b project_root)))})

;; =============================================================================
;; MCP Interface
;; =============================================================================

(defn handle-lsp
  "MCP tool handler for LSP commands. Dispatches on :command key.
   Returns MCP-compatible response map with :content vector."
  [{:keys [command] :as params}]
  (if-let [handler (get command-handlers command)]
    (try
      (let [result (handler params)]
        (if (:isError result)
          result
          {:content [{:type "text" :text (pr-str result)}]}))
      (catch Exception e
        (log/error e "LSP command failed:" command)
        {:content [{:type "text" :text (pr-str {:error   "Failed to handle command"
                                                :command command
                                                :details (ex-message e)})}]
         :isError true}))
    {:content [{:type "text" :text (pr-str {:error     "Unknown command"
                                            :command   command
                                            :available (sort (keys command-handlers))})}]
     :isError true}))

(defn tool-def
  "MCP tool definition for the LSP tool."
  []
  {:name        "lsp"
   :description (str "Clojure LSP analysis and KG sync tools. "
                     "Static analysis: analyze, definitions, calls, ns-graph, callers, references, sync, status. "
                     "Live LSP bridge: bridge-status, hover, definition, live-references, "
                     "symbols, cursor-info, server-info, workspaces.")
   :inputSchema {:type       "object"
                 :properties {:command      {:type "string"
                                             :enum (sort (keys command-handlers))}
                              :project_root {:type        "string"
                                             :description "Path to the project root directory"}
                              :project_id   {:type        "string"
                                             :description "Project identifier for KG sync"}
                              :scope        {:type        "string"
                                             :description "Scope for KG sync operations"}
                              :namespace    {:type        "string"
                                             :description "Filter by namespace (e.g., my.app.core)"}
                              :function     {:type        "string"
                                             :description "Filter by function name"}
                              :file_path    {:type        "string"
                                             :description "Path to file (live bridge commands)"}
                              :line         {:type        "integer"
                                             :description "0-based line number (live bridge commands)"}
                              :column       {:type        "integer"
                                             :description "0-based column number (live bridge commands)"}}
                 :required   ["command"]}})

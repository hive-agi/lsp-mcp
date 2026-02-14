(ns lsp-mcp.tools
  "MCP tool handlers for LSP analysis.

   Uses Docker sidecar cache (via lsp-mcp.cache) for fast reads.
   Falls back to in-process clojure-lsp when cache unavailable.

   Request-level memoization: multiple commands in quick succession
   (e.g., definitions then calls) share the same analysis result
   via a 30-second TTL cache keyed by project_root."
  (:require [lsp-mcp.core :as core]
            [lsp-mcp.cache :as cache]
            [lsp-mcp.analysis :as analysis]
            [lsp-mcp.log :as log]))

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
;; Command Handlers
;; =============================================================================

(def ^:private command-handlers
  {"analyze"     (fn [{:keys [project_root]}]
                   (let [result   (cached-analyze project_root)
                         vars     (analysis/extract-var-definitions (:analysis result))
                         ns-graph (analysis/extract-namespace-graph (:dep-graph result))]
                     {:num-files      (count (:analysis result))
                      :num-namespaces (count ns-graph)
                      :num-vars       (count vars)
                      :cache-status   (cache/cache-status)}))
   "definitions" (fn [{:keys [project_root namespace]}]
                   (let [result (cached-analyze project_root)
                         vars   (analysis/extract-var-definitions (:analysis result))]
                     (if namespace
                       (vec (filter #(= (str (:ns %)) namespace) vars))
                       vars)))
   "calls"       (fn [{:keys [project_root namespace function]}]
                   (let [result (cached-analyze project_root)
                         calls  (analysis/extract-call-graph (:analysis result))]
                     (cond->> calls
                       namespace (filter #(= (str (:caller-ns %)) namespace))
                       function  (filter #(= (str (:caller-fn %)) function))
                       true      vec)))
   "ns-graph"    (fn [{:keys [project_root]}]
                   (let [result (cached-analyze project_root)]
                     (analysis/extract-namespace-graph (:dep-graph result))))
   "sync"        (fn [{:keys [project_root project_id scope]}]
                   (core/analyze-and-sync! project_root project_id scope))
   "status"      (fn [_]
                   {:bridge-available? (some? (try (requiring-resolve 'hive-mcp.knowledge-graph.edges/add-edge!)
                                                   (catch Exception _ nil)))
                    :cache             (cache/cache-status)})
   "callers"     (fn [{:keys [project_root function namespace]}]
                   (let [result (cached-analyze project_root)
                         calls  (analysis/extract-call-graph (:analysis result))]
                     (->> calls
                          (filter (fn [c]
                                    (and (or (nil? function)  (= (str (:callee-fn c)) function))
                                         (or (nil? namespace) (= (str (:callee-ns c)) namespace)))))
                          vec)))
   "references"  (fn [{:keys [project_root function namespace]}]
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
                                   :caller-fn (:caller-fn c)})))))})

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
        {:content [{:type "text" :text (pr-str result)}]})
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
   :description "Clojure LSP analysis and KG sync tools"
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
                                             :description "Filter by function name"}}
                 :required   ["command"]}})

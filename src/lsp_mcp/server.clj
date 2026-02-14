(ns lsp-mcp.server
  "Standalone babashka MCP server for clojure-lsp analysis.

   Reads from Docker sidecar cache (~/.cache/hive-lsp/<project>/)
   and exposes LSP analysis tools via MCP stdio protocol.

   Start: bb --config bb.edn server"
  (:require [modex-bb.mcp.server :as mcp-server]
            [modex-bb.mcp.tools :refer [tools]]
            [lsp-mcp.cache :as cache]
            [lsp-mcp.analysis :as analysis]
            [lsp-mcp.log :as log]))

;; =============================================================================
;; Request-Level Memoization (30s TTL)
;; =============================================================================

(def ^:private analysis-cache
  "Single-entry TTL cache for analysis results.
   Avoids re-parsing large EDN on rapid multi-tool calls."
  (atom nil))

(def ^:private cache-ttl-ms 30000)

(defn- cached-analyze
  "Analyze with 30s TTL memoization keyed by project_root."
  [project-root]
  (let [now    (System/currentTimeMillis)
        cached @analysis-cache]
    (if (and cached
             (= project-root (:project-root cached))
             (< (- now (:timestamp-ms cached)) cache-ttl-ms))
      (do (log/debug "Analysis cache hit for" project-root)
          (:result cached))
      (let [result (analysis/analyze-project! project-root)]
        (log/debug "Analysis cache miss for" project-root)
        (reset! analysis-cache {:project-root project-root
                                :result       result
                                :timestamp-ms now})
        result))))

;; =============================================================================
;; Tool Definitions
;; =============================================================================

(def lsp-tools
  (tools
   (analyze "Project analysis stats: namespace count, var count, cache status"
            [{:keys [project_root]
              :type {:project_root :string}
              :doc  {:project_root "Path to the project root directory"}}]
            (let [result   (cached-analyze project_root)
                  vars     (analysis/extract-var-definitions (:analysis result))
                  ns-graph (analysis/extract-namespace-graph (:dep-graph result))]
              [{:num-files      (count (:analysis result))
                :num-namespaces (count ns-graph)
                :num-vars       (count vars)
                :cache-status   (cache/cache-status)}]))

   (definitions "Var definitions, filterable by namespace"
     [{:keys [project_root namespace]
       :type {:project_root :string :namespace :string}
       :doc  {:project_root "Path to the project root directory"
              :namespace    "Filter by namespace (e.g. my.app.core)"}
       :or   {namespace nil}}]
     (let [result (cached-analyze project_root)
           vars   (analysis/extract-var-definitions (:analysis result))]
       [(if namespace
          (vec (filter #(= (str (:ns %)) namespace) vars))
          vars)]))

   (calls "Call graph edges, filterable by namespace/function"
          [{:keys [project_root namespace function]
            :type {:project_root :string :namespace :string :function :string}
            :doc  {:project_root "Path to the project root directory"
                   :namespace    "Filter by caller namespace"
                   :function     "Filter by caller function name"}
            :or   {namespace nil function nil}}]
          (let [result (cached-analyze project_root)
                calls  (analysis/extract-call-graph (:analysis result))]
            [(cond->> calls
               namespace (filter #(= (str (:caller-ns %)) namespace))
               function  (filter #(= (str (:caller-fn %)) function))
               true      vec)]))

   (callers "Reverse call lookup — who calls a given function"
            [{:keys [project_root function namespace]
              :type {:project_root :string :function :string :namespace :string}
              :doc  {:project_root "Path to the project root directory"
                     :function     "Callee function name to look up"
                     :namespace    "Callee namespace to filter"}
              :or   {function nil namespace nil}}]
            (let [result (cached-analyze project_root)
                  calls  (analysis/extract-call-graph (:analysis result))]
              [(->> calls
                    (filter (fn [c]
                              (and (or (nil? function)  (= (str (:callee-fn c)) function))
                                   (or (nil? namespace) (= (str (:callee-ns c)) namespace)))))
                    vec)]))

   (references "References with file/row info for a function"
               [{:keys [project_root function namespace]
                 :type {:project_root :string :function :string :namespace :string}
                 :doc  {:project_root "Path to the project root directory"
                        :function     "Function name to find references for"
                        :namespace    "Namespace to filter"}
                 :or   {function nil namespace nil}}]
               (let [result (cached-analyze project_root)
                     calls  (analysis/extract-call-graph (:analysis result))]
                 [(->> calls
                       (filter (fn [c]
                                 (and (or (nil? function)  (= (str (:callee-fn c)) function))
                                      (or (nil? namespace) (= (str (:callee-ns c)) namespace)))))
                       (mapv (fn [c]
                               {:file      (:file c)
                                :row       (:row c)
                                :caller-ns (:caller-ns c)
                                :caller-fn (:caller-fn c)})))]))

   (ns_graph "Namespace dependency graph"
             [{:keys [project_root]
               :type {:project_root :string}
               :doc  {:project_root "Path to the project root directory"}}]
             (let [result (cached-analyze project_root)]
               [(analysis/extract-namespace-graph (:dep-graph result))]))

   (status "Cache status overview"
           [{:keys []
             :type {}
             :doc  {}}]
           (let [bridge? (some? (try (requiring-resolve 'hive-mcp.knowledge-graph.edges/add-edge!)
                                     (catch Exception _ nil)))]
             [{:bridge-available? bridge?
               :cache             (cache/cache-status)}]))

   (list_projects "List all projects with cached analysis"
                  [{:keys []
                    :type {}
                    :doc  {}}]
                  [(cache/list-cached-projects)])))

;; =============================================================================
;; Server
;; =============================================================================

(def mcp-server
  (mcp-server/->server
   {:name    "lsp-mcp"
    :version "0.1.0"
    :tools   lsp-tools}))

(defn -main
  "Start the LSP MCP server.
   Reads JSON-RPC from stdin, writes responses to stdout."
  [& _args]
  (log/info "Starting lsp-mcp server v0.1.0")
  (mcp-server/start-server! mcp-server)
  ;; Allow in-flight response futures to flush before exit.
  ;; bb's shutdown-agents doesn't wait for pending futures.
  (Thread/sleep 500))

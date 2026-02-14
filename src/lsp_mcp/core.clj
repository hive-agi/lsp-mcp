(ns lsp-mcp.core
  "Public API for LSP to KG pipeline orchestration.

   analyze        — returns raw analysis result (cache-first)
   analyze-and-sync! — full pipeline: analyze → extract → transform → sync to KG
   status         — bridge + cache status"
  (:require
   [lsp-mcp.analysis :as analysis]
   [lsp-mcp.cache :as cache]
   [lsp-mcp.transform :as transform]
   [lsp-mcp.kg-bridge :as bridge]
   [lsp-mcp.log :as log]))

(defn analyze
  "Analyze a Clojure project using clojure-lsp (cache-first).

   Returns the raw analysis result map with :analysis and :dep-graph keys.
   Callers use analysis/extract-* fns on the result as needed."
  [project-root]
  (analysis/analyze-project! project-root))

(defn analyze-and-sync!
  "Orchestrate full analysis and sync to KG.

   Pipeline: analyze-project! → extract var-defs/call-graph/ns-graph
             → transform to KG operations → sync to KG via bridge.

   Returns {:analysis-stats {...} :sync-stats {...}}."
  [project-root project-id scope]
  (log/info "Starting analysis and sync for project-root:" project-root
            "project-id:" project-id)
  (let [start-time  (System/nanoTime)
        raw         (analyze project-root)
        analysis-ms (/ (- (System/nanoTime) start-time) 1e6)
        _           (log/info "Analysis completed in" analysis-ms "ms")
        ;; Extract structured data from raw analysis
        var-defs    (analysis/extract-var-definitions (:analysis raw))
        call-graph  (analysis/extract-call-graph (:analysis raw))
        ns-graph    (analysis/extract-namespace-graph (:dep-graph raw))
        ;; Transform to KG operations
        operations  (transform/analysis->kg-operations project-id
                                                       var-defs
                                                       call-graph
                                                       ns-graph)
        ;; Sync to KG
        sync-start  (System/nanoTime)
        sync-result (bridge/sync-to-kg! project-id operations scope)
        sync-ms     (/ (- (System/nanoTime) sync-start) 1e6)]
    (log/info "Sync completed in" sync-ms "ms")
    {:analysis-stats {:time-ms  analysis-ms
                      :var-defs (count var-defs)
                      :calls    (count call-graph)
                      :nses     (count ns-graph)}
     :sync-stats     {:time-ms sync-ms
                      :result  sync-result}}))

(defn status
  "Return status information about the LSP bridge and cache."
  []
  {:bridge-available? (bridge/available?)
   :cache             (cache/cache-status)})

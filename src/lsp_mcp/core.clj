(ns lsp-mcp.core
  "Public API for LSP to KG pipeline orchestration.

   Railway-oriented: every step returns a hive-dsl Result. The pipeline
   short-circuits on the first error. Independent extraction steps run
   concurrently via hive-weave fork-join.

   analyze            — returns Result wrapping raw analysis (cache-first)
   analyze-and-sync!  — full pipeline: analyze → extract (parallel) →
                        transform → sync to KG
   status             — bridge + cache status"
  (:require
   [hive-dsl.result :as r :refer [let-ok ok err]]
   [hive-weave.parallel :as wp]
   [lsp-mcp.analysis :as analysis]
   [lsp-mcp.cache :as cache]
   [lsp-mcp.kg-bridge :as bridge]
   [lsp-mcp.log :as log]
   [lsp-mcp.transform :as transform]
   [lsp-mcp.sidecar :as sidecar]))

;; =============================================================================
;; Public Steps
;; =============================================================================

(defn analyze
  "Analyze a Clojure project using clojure-lsp (cache-first).
   Returns a Result ({:ok analysis-map} | {:error category ...})."
  [project-root]
  (analysis/analyze-project! project-root))

;; =============================================================================
;; Pipeline Helpers (pure where possible)
;; =============================================================================

(defn- extract-all
  "Run the three extract-* fns concurrently with a shared time budget.
   Pre-extracted (sidecar fast path) bypasses fork-join.

   Returns {:var-defs ... :call-graph ... :ns-graph ...}."
  [raw]
  (if (:pre-extracted raw)
    {:var-defs   (:var-defs raw)
     :call-graph (:call-graph raw)
     :ns-graph   (analysis/extract-namespace-graph (:dep-graph raw))}
    (wp/fork-join
     {:budget-ms 30000}
     [:var-defs   #(analysis/extract-var-definitions (:analysis raw)) []]
     [:call-graph #(analysis/extract-call-graph (:analysis raw))      []]
     [:ns-graph   #(analysis/extract-namespace-graph (:dep-graph raw)) []])))

(defn- now-ns [] (System/nanoTime))
(defn- elapsed-ms [start] (/ (- (now-ns) start) 1e6))

;; =============================================================================
;; Full Pipeline
;; =============================================================================

(defn analyze-and-sync!
  "Orchestrate full analysis and sync to KG. Railway-oriented.

   Pipeline: analyze → extract (parallel) → transform → sync.
   Short-circuits on first error.

   Returns Result:
     {:ok {:analysis-stats {...} :sync-stats {...}}}
     {:error category ...}"
  [project-root project-id scope]
  (log/info "Starting analysis and sync for project-root:" project-root
            "project-id:" project-id)
  (let [t0 (now-ns)]
    (let-ok [raw         (analyze project-root)
             extracted   (ok (extract-all raw))
             analysis-ms (ok (elapsed-ms t0))
             _           (ok (log/info "Analysis+extract completed in" analysis-ms "ms"))
             {:keys [var-defs call-graph ns-graph]} (ok extracted)
             operations  (ok (transform/analysis->kg-operations
                              project-id var-defs call-graph ns-graph))
             sync-t0     (ok (now-ns))
             sync-result (bridge/sync-to-kg! project-id operations scope)
             sync-ms     (ok (elapsed-ms sync-t0))]
      (log/info "Sync completed in" sync-ms "ms"
                (when (:pre-extracted raw) "(pre-extracted fast path)"))
      (ok {:analysis-stats {:time-ms       analysis-ms
                            :var-defs      (count var-defs)
                            :calls         (count call-graph)
                            :nses          (count ns-graph)
                            :pre-extracted (boolean (:pre-extracted raw))}
           :sync-stats     {:time-ms sync-ms
                            :result  sync-result}}))))

(defn status
  "Return bridge, functional sidecar, and cache status."
  []
  {:bridge-available? (bridge/available?)
   :sidecar (sidecar/health)
   :cache (cache/cache-status)})

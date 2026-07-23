(ns lsp-mcp.tools
  "MCP tool handlers for LSP analysis.

   Three strategies:
   1. Docker sidecar cache (via lsp-mcp.cache) — fast reads
   2. In-process clojure-lsp — fallback when cache unavailable
   3. Live Emacs LSP bridge (via lsp-mcp.emacs-bridge) — real-time queries

   Railway-oriented: handlers chain Result-returning steps via let-ok.
   Errors short-circuit and surface as MCP error responses.

   Request-level memoization: multiple commands in quick succession
   (e.g., definitions then calls) share the same analysis Result
   via a 30-second TTL cache keyed by project_root."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r :refer [let-ok ok err]]
            [lsp-mcp.analysis :as analysis]
            [lsp-mcp.bridge :as bridge]
            [lsp-mcp.cache :as cache]
            [lsp-mcp.core :as core]
            [lsp-mcp.log :as log]
            [lsp-mcp.sidecar :as sidecar]))

;; =============================================================================
;; Request-Level Memoization (30s TTL)
;; =============================================================================

(def ^:private analysis-cache
  "Atom holding {:project-root str :result Result :timestamp-ms long}.
   Single-entry cache — only the most recent project is cached."
  (atom nil))

(def ^:private cache-ttl-ms
  "TTL for memoized analysis results: 30 seconds."
  30000)

(defn- cached-analyze
  "Analyze with 30s TTL memoization. Returns a Result.
   Cache is keyed by project-root; stale or mismatched entries are evicted."
  [project-root]
  (let [now    (System/currentTimeMillis)
        cached @analysis-cache]
    (if (and cached
             (= project-root (:project-root cached))
             (< (- now (:timestamp-ms cached)) cache-ttl-ms))
      (do (log/debug "Analysis cache hit for" project-root)
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
  "Get the active ILspBridge instance, or return err Result."
  []
  (if-let [b @bridge-instance]
    (ok b)
    (err :bridge/unavailable {:message "Emacs LSP bridge not available"})))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- require-project-root
  "Validate project_root param. Returns ok Result or err."
  [params]
  (if (str/blank? (:project_root params))
    (err :params/missing-root
         {:message "project_root is required"
          :command (:command params)})
    (ok (:project_root params))))

(defn- with-analysis
  "Run f on the unwrapped analysis result. f receives the raw map.
   Threads project-root through validate → cached-analyze → f."
  [params f]
  (let-ok [root (require-project-root params)
           raw  (cached-analyze root)]
    (ok (f raw))))

;; =============================================================================
;; Command Handlers — each returns a Result
;; =============================================================================

(defn- h-analyze [params]
  (with-analysis params
    (fn [raw]
      (let [vars     (analysis/extract-var-definitions (:analysis raw))
            ns-graph (analysis/extract-namespace-graph (:dep-graph raw))]
        {:num-files      (count (:analysis raw))
         :num-namespaces (count ns-graph)
         :num-vars       (count vars)
         :cache-status   (cache/cache-status)}))))

(defn- h-definitions [{:keys [namespace] :as params}]
  (with-analysis params
    (fn [raw]
      (let [vars (analysis/extract-var-definitions (:analysis raw))]
        (if namespace
          (vec (filter #(= (str (:ns %)) namespace) vars))
          vars)))))

(defn- h-calls [{:keys [namespace function] :as params}]
  (with-analysis params
    (fn [raw]
      (let [calls (analysis/extract-call-graph (:analysis raw))]
        (cond->> calls
          namespace (filter #(= (str (:caller-ns %)) namespace))
          function  (filter #(= (str (:caller-fn %)) function))
          true      vec)))))

(defn- h-ns-graph
  "Namespace dependency graph.

   Optional params (programmatic budget):
     :namespace  filter to subgraph reachable from this ns root
     :depth      BFS depth from :namespace (default 2; ignored if :namespace nil)
     :max-nodes  cap on returned node count (default 100; suppressed if :raw true)
     :raw        when true, bypass cap and return full graph

   Returns:
     {:ns-graph [{:ns sym :depends-on #{sym} :dependents #{sym} :internal? bool}]
      :total    int   ;; full project node count
      :returned int   ;; node count in response
      :truncated? bool
      :hint?    str   ;; populated only when truncated"
  [{:keys [namespace depth max-nodes raw] :as params}]
  (with-analysis params
    (fn [raw-data]
      (let [full       (analysis/extract-namespace-graph (:dep-graph raw-data))
            total      (count full)
            depth      (or depth 2)
            max-nodes  (or max-nodes 100)
            ;; BFS to subgraph if root namespace given
            subgraph   (if (str/blank? namespace)
                         full
                         (let [by-ns (into {} (map (juxt (comp str :ns) identity)) full)
                               root  (str namespace)]
                           (loop [frontier #{root}
                                  seen     #{}
                                  d        0
                                  acc      []]
                             (if (or (empty? frontier) (> d depth))
                               acc
                               (let [next-acc  (into acc
                                                     (keep #(get by-ns %))
                                                     frontier)
                                     seen'     (into seen frontier)
                                     neighbors (->> frontier
                                                    (mapcat (fn [n]
                                                              (let [entry (get by-ns n)]
                                                                (concat (map str (:depends-on entry))
                                                                        (map str (:dependents entry))))))
                                                    (remove seen')
                                                    set)]
                                 (recur neighbors seen' (inc d) next-acc))))))
            n          (count subgraph)
            truncated? (and (not raw) (> n max-nodes))
            returned   (if truncated? (vec (take max-nodes subgraph)) (vec subgraph))]
        (cond-> {:ns-graph returned
                 :total    total
                 :returned (count returned)
                 :truncated? truncated?}
          truncated?
          (assoc :hint
                 (str "Result truncated to " max-nodes " of " n
                      " nodes. Pass :raw true for full graph, "
                      ":max-nodes N for a different cap, "
                      "or :namespace + :depth to scope the BFS.")))))))

(defn- h-callers [{:keys [function namespace] :as params}]
  (with-analysis params
    (fn [raw]
      (->> (analysis/extract-call-graph (:analysis raw))
           (filter (fn [c]
                     (and (or (nil? function)  (= (str (:callee-fn c)) function))
                          (or (nil? namespace) (= (str (:callee-ns c)) namespace)))))
           vec))))

(defn- h-references [{:keys [function namespace] :as params}]
  (with-analysis params
    (fn [raw]
      (->> (analysis/extract-call-graph (:analysis raw))
           (filter (fn [c]
                     (and (or (nil? function)  (= (str (:callee-fn c)) function))
                          (or (nil? namespace) (= (str (:callee-ns c)) namespace)))))
           (mapv (fn [c]
                   {:file      (:file c)
                    :row       (:row c)
                    :caller-ns (:caller-ns c)
                    :caller-fn (:caller-fn c)}))))))

(defn- h-sync [{:keys [project_root project_id scope]}]
  (if (str/blank? project_root)
    (err :params/missing-root {:message "project_root is required"})
    (core/analyze-and-sync! project_root project_id scope)))

(defn- h-status [_]
  (ok {:bridge-available?
       (some? (r/rescue nil
                        (requiring-resolve
                         'hive-mcp.knowledge-graph.edges/add-edge!)))
       :sidecar (sidecar/health)
       :cache (cache/cache-status)}))

(defn- h-job-status
  [{:keys [project_id job_id]}]
  (if (str/blank? project_id)
    (err :params/missing-project-id
         {:message "project_id is required"})
    (let [result (sidecar/job-status project_id job_id)]
      (if (:error result)
        (err (:error result) (dissoc result :error))
        (ok result)))))

(defn- h-cancel
  [{:keys [project_id job_id]}]
  (if (str/blank? project_id)
    (err :params/missing-project-id
         {:message "project_id is required"})
    (let [result (sidecar/cancel-analysis! project_id job_id)]
      (if (:error result)
        (err (:error result) (dissoc result :error))
        (ok result)))))

;; --- Live LSP bridge handlers ------------------------------------------------

(defn- with-bridge
  "Run f on resolved bridge instance, returning Result."
  [f]
  (let-ok [b (resolve-bridge)]
    (ok (f b))))

(defn- h-bridge-status [_] (with-bridge bridge/bridge-status))
(defn- h-workspaces    [_] (with-bridge bridge/bridge-workspaces))
(defn- h-hover [{:keys [project_root file_path line column]}]
  (with-bridge #(bridge/bridge-hover % project_root file_path line column)))
(defn- h-definition [{:keys [project_root file_path line column]}]
  (with-bridge #(bridge/bridge-definition % project_root file_path line column)))
(defn- h-live-references [{:keys [project_root file_path line column]}]
  (with-bridge #(bridge/bridge-references % project_root file_path line column)))
(defn- h-symbols [{:keys [project_root file_path]}]
  (with-bridge #(bridge/bridge-document-symbols % project_root file_path)))
(defn- h-cursor-info [{:keys [project_root file_path line column]}]
  (with-bridge #(bridge/bridge-cursor-info % project_root file_path line column)))
(defn- h-server-info [{:keys [project_root]}]
  (with-bridge #(bridge/bridge-server-info % project_root)))

(def ^:private command-handlers
  {"analyze"         h-analyze
   "definitions"     h-definitions
   "calls"           h-calls
   "ns-graph"        h-ns-graph
   "sync"            h-sync
   "status"          h-status
   "job-status"      h-job-status
   "cancel"          h-cancel
   "callers"         h-callers
   "references"      h-references
   "bridge-status"   h-bridge-status
   "workspaces"      h-workspaces
   "hover"           h-hover
   "definition"      h-definition
   "live-references" h-live-references
   "symbols"         h-symbols
   "cursor-info"     h-cursor-info
   "server-info"     h-server-info})

;; =============================================================================
;; MCP Adaptor — Result -> MCP response
;; =============================================================================

(defn- result->mcp
  "Convert a hive-dsl Result into an MCP tool response."
  [command result]
  (cond
    (r/ok? result)
    {:content [{:type "text" :text (pr-str (:ok result))}]}

    (r/err? result)
    {:content [{:type "text" :text (pr-str {:error   (:error result)
                                            :command command
                                            :details (dissoc result :error)})}]
     :isError true}

    :else
    {:content [{:type "text" :text (pr-str result)}]}))

(defn handle-lsp
  "MCP tool handler for LSP commands. Dispatches on :command key.
   Each handler returns a Result; the Result is adapted to an MCP response."
  [{:keys [command] :as params}]
  (if-let [handler (get command-handlers command)]
    (try
      (result->mcp command (handler params))
      (catch Exception e
        (log/error e "LSP command failed:" command)
        (result->mcp command
                     (err :handler/exception
                          {:message (ex-message e)}))))
    (result->mcp command
                 (err :handler/unknown-command
                      {:available (sort (keys command-handlers))}))))

(defn tool-def
  "MCP tool definition for the LSP tool."
  []
  {:name "lsp"
   :description
   (str "Clojure LSP analysis, sidecar lifecycle, and KG sync tools. "
        "Static analysis: analyze, definitions, calls, ns-graph, callers, "
        "references, sync, status, job-status, cancel. "
        "Live LSP bridge: bridge-status, hover, definition, live-references, "
        "symbols, cursor-info, server-info, workspaces.")
   :inputSchema
   {:type "object"
    :properties
    {:command {:type "string"
               :enum (sort (keys command-handlers))}
     :project_root {:type "string"
                    :description "Path to the project root directory"}
     :project_id {:type "string"
                  :description "Sidecar project identifier or KG sync id"}
     :job_id {:type "string"
              :description "Optional sidecar job identity guard"}
     :scope {:type "string"
             :description "Scope for KG sync operations"}
     :namespace {:type "string"
                 :description "Filter by namespace (e.g., my.app.core)"}
     :function {:type "string"
                :description "Filter by function name"}
     :file_path {:type "string"
                 :description "Path to file (live bridge commands)"}
     :line {:type "integer"
            :description "0-based line number (live bridge commands)"}
     :column {:type "integer"
              :description "0-based column number (live bridge commands)"}}
    :required ["command"]}})

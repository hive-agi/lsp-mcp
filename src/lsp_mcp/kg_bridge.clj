(ns lsp-mcp.kg-bridge
  "KG bridge — emits LSP analysis results to hive-mcp KG via requiring-resolve.

   Railway-oriented: sync-to-kg! returns a hive-dsl Result.
   Hot path uses hive-weave bounded-pmap for parallel writes.

   Resolved symbols (lazy, no compile-time dep on hive-mcp):
     hive-mcp.vectordb.facade/index-memory-entry!     — store memory entry, returns entry-id
     hive-mcp.vectordb.facade/content-hash            — SHA-256 for dedup
     hive-mcp.vectordb.facade/find-duplicate          — idempotent upsert check
     hive-mcp.knowledge-graph.edges/add-edge!         — create KG edge, returns edge-id
     hive-mcp.tools.memory.scope/inject-project-scope — add scope tag to tags vec"
  (:require [hive-dsl.result :as r]
            [hive-weave.parallel :as wp]
            [lsp-mcp.log :as log]))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:private write-concurrency
  "Max parallel writes per phase (memory entries / KG edges)."
  8)

(def ^:private write-timeout-ms
  "Per-item write timeout."
  10000)

;; =============================================================================
;; Symbol Resolution (lazy — no compile-time coupling)
;; =============================================================================

(defn- resolve-fn
  "Resolve a symbol via requiring-resolve. Returns the var's fn or nil."
  [sym]
  (try
    (requiring-resolve sym)
    (catch Exception e
      (log/debug "Failed to resolve:" sym (ex-message e))
      nil)))

;; =============================================================================
;; Memory Entry Bridge — Result-returning
;; =============================================================================

(defn- add-memory-entry!
  "Add a memory entry to hive-mcp via requiring-resolve.

   Returns Result:
     {:ok {:key str :id str}}    on success
     {:error :bridge/* {...}}    on failure / unavailable"
  [entry project-id]
  (if-let [index-fn (resolve-fn 'hive-mcp.vectordb.facade/index-memory-entry!)]
    (r/try-effect* :bridge/index-failed
      (let [inject-fn (resolve-fn 'hive-mcp.tools.memory.scope/inject-project-scope)
            tags      (cond-> (vec (:tags entry))
                        inject-fn (inject-fn project-id))
            hash-fn   (resolve-fn 'hive-mcp.vectordb.facade/content-hash)
            c-hash    (when hash-fn (hash-fn (:content entry)))
            dup-fn    (resolve-fn 'hive-mcp.vectordb.facade/find-duplicate)
            existing  (when (and dup-fn c-hash)
                        (dup-fn (:type entry) c-hash :project-id project-id))
            id        (if existing
                        (do (log/debug "Duplicate entry, reusing:" (:id existing))
                            (:id existing))
                        (index-fn (cond-> {:type       (:type entry)
                                           :content    (:content entry)
                                           :tags       tags
                                           :duration   (:duration entry)
                                           :project-id project-id}
                                    c-hash (assoc :content-hash c-hash))))]
        {:key (:key entry) :id id}))
    (r/err :bridge/unavailable
           {:message "hive-mcp.vectordb.facade/index-memory-entry! not resolvable"})))

;; =============================================================================
;; KG Edge Bridge — Result-returning
;; =============================================================================

(defn- add-kg-edge!
  "Add a KG edge to hive-mcp via requiring-resolve. Returns Result."
  [edge-map scope]
  (if-let [edge-fn (resolve-fn 'hive-mcp.knowledge-graph.edges/add-edge!)]
    (r/try-effect* :bridge/edge-failed
      (edge-fn {:from        (:from edge-map)
                :to          (:to edge-map)
                :relation    (:relation edge-map)
                :scope       scope
                :confidence  (get edge-map :confidence 1.0)
                :source-type (get edge-map :source-type :automated)
                :created-by  (get edge-map :created-by "lsp-mcp")}))
    (r/err :bridge/unavailable
           {:message "hive-mcp.knowledge-graph.edges/add-edge! not resolvable"})))

;; =============================================================================
;; Edge Resolution (key → memory-id)
;; =============================================================================

(defn- resolve-edge-node-ids
  "Resolve edge's :from-key and :to-key to memory-ids using key->id-map."
  [key->id-map edge]
  (let [from-id (get key->id-map (:from-key edge))
        to-id   (get key->id-map (:to-key edge))]
    (when (and from-id to-id)
      (assoc edge :from from-id :to to-id))))

;; =============================================================================
;; Phase 1 — Memory entries (bounded-parallel)
;; =============================================================================

(defn- index-entries!
  "Run Phase 1: index every memory entry in parallel (bounded).
   Returns {:key->id {key id} :errors [str]}."
  [memory-entries project-id]
  (let [results (wp/bounded-pmap
                 {:concurrency write-concurrency
                  :timeout-ms  write-timeout-ms
                  :fallback    (r/err :bridge/timeout {:phase :index})}
                 #(add-memory-entry! % project-id)
                 memory-entries)]
    (reduce (fn [acc [entry result]]
              (if (r/ok? result)
                (let [{:keys [key id]} (:ok result)]
                  (update acc :key->id assoc key id))
                (update acc :errors conj (str "Failed entry: " (:key entry)
                                              " (" (:error result) ")"))))
            {:key->id {} :errors []}
            (map vector memory-entries results))))

;; =============================================================================
;; Phase 2 — KG edges (bounded-parallel)
;; =============================================================================

(defn- write-edges!
  "Run Phase 2: write every resolvable KG edge in parallel (bounded).
   Returns {:edge-count n :errors [str]}."
  [kg-edges key->id scope]
  (let [resolvable (keep #(when-let [r (resolve-edge-node-ids key->id %)]
                            [% r])
                         kg-edges)
        results    (wp/bounded-pmap
                    {:concurrency write-concurrency
                     :timeout-ms  write-timeout-ms
                     :fallback    (r/err :bridge/timeout {:phase :edges})}
                    (fn [[_ resolved]] (add-kg-edge! resolved scope))
                    resolvable)]
    (reduce (fn [acc [[orig _] result]]
              (if (r/ok? result)
                (update acc :edge-count inc)
                (update acc :errors conj
                        (str "Failed edge: " (:from-key orig)
                             " -> " (:to-key orig)
                             " (" (:error result) ")"))))
            {:edge-count 0 :errors []}
            (map vector resolvable results))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn sync-to-kg!
  "Sync LSP analysis operations to hive-mcp KG. Returns a Result.

   operations - map from analysis->kg-operations:
     :memory-entries — vec of {:type :content :tags :duration :key}
     :kg-edges       — vec of {:from-key :to-key :relation :confidence :source-type}
     :stats          — {:fns N :edges N :namespaces N}

   Returns:
     {:ok {:created N :edges N :errors [str]}}  on success (errors vec may be non-empty)
     {:error :bridge/unavailable {...}}         when bridge missing"
  [project-id operations scope]
  (let [{:keys [memory-entries kg-edges]} operations
        phase1 (index-entries! memory-entries project-id)
        phase2 (write-edges! kg-edges (:key->id phase1) scope)]
    (r/ok {:created (count (:key->id phase1))
           :edges   (:edge-count phase2)
           :errors  (into (:errors phase1) (:errors phase2))})))

(defn available?
  "Check if hive-mcp functions are resolvable (vectordb + KG edges)."
  []
  (boolean
   (and (resolve-fn 'hive-mcp.vectordb.facade/index-memory-entry!)
        (resolve-fn 'hive-mcp.knowledge-graph.edges/add-edge!))))

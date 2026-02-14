(ns lsp-mcp.kg-bridge
  "KG bridge — emits LSP analysis results to hive-mcp KG via requiring-resolve.

   Resolved symbols (lazy, no compile-time dep on hive-mcp):
     hive-mcp.chroma/index-memory-entry!     — store memory entry, returns entry-id
     hive-mcp.chroma/content-hash            — SHA-256 for dedup
     hive-mcp.chroma/find-duplicate          — idempotent upsert check
     hive-mcp.knowledge-graph.edges/add-edge! — create KG edge, returns edge-id
     hive-mcp.tools.memory.scope/inject-project-scope — add scope tag to tags vec"
  (:require [lsp-mcp.log :as log]))

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
;; Memory Entry Bridge
;; =============================================================================

(defn- add-memory-entry!
  "Add a memory entry to hive-mcp Chroma via requiring-resolve.

   Uses chroma/index-memory-entry! directly (returns entry-id string).
   Performs content-hash dedup when chroma/find-duplicate is available.

   Returns entry-id string or nil on failure."
  [entry project-id]
  (when-let [index-fn (resolve-fn 'hive-mcp.chroma/index-memory-entry!)]
    (try
      (let [;; Scope injection — adds 'scope:project:<id>' tag
            inject-fn   (resolve-fn 'hive-mcp.tools.memory.scope/inject-project-scope)
            tags        (cond-> (vec (:tags entry))
                          inject-fn (inject-fn project-id))
            ;; Content-hash for dedup
            hash-fn     (resolve-fn 'hive-mcp.chroma/content-hash)
            c-hash      (when hash-fn (hash-fn (:content entry)))
            ;; Check for existing duplicate
            dup-fn      (resolve-fn 'hive-mcp.chroma/find-duplicate)
            existing    (when (and dup-fn c-hash)
                          (dup-fn (:type entry) c-hash :project-id project-id))]
        (if existing
          (do (log/debug "Duplicate entry, reusing:" (:id existing))
              (:id existing))
          ;; Index new entry — returns entry-id string directly
          (index-fn (cond-> {:type       (:type entry)
                             :content    (:content entry)
                             :tags       tags
                             :duration   (:duration entry)
                             :project-id project-id}
                      c-hash (assoc :content-hash c-hash)))))
      (catch Exception e
        (log/error "Failed to add memory entry:" (ex-message e))
        nil))))

;; =============================================================================
;; KG Edge Bridge
;; =============================================================================

(defn- add-kg-edge!
  "Add a KG edge to hive-mcp via requiring-resolve.

   add-edge! returns the edge-id string directly (not a map).
   Returns edge-id string or nil on failure."
  [edge-map scope]
  (when-let [edge-fn (resolve-fn 'hive-mcp.knowledge-graph.edges/add-edge!)]
    (try
      ;; add-edge! returns edge-id string directly
      (edge-fn {:from        (:from edge-map)
                :to          (:to edge-map)
                :relation    (:relation edge-map)
                :scope       scope
                :confidence  (get edge-map :confidence 1.0)
                :source-type (get edge-map :source-type :automated)
                :created-by  (get edge-map :created-by "lsp-mcp")})
      (catch Exception e
        (log/error "Failed to add KG edge:" (ex-message e))
        nil))))

;; =============================================================================
;; Edge Resolution (key → memory-id)
;; =============================================================================

(defn- resolve-edge-node-ids
  "Resolve edge's :from-key and :to-key to memory-ids using the key->id-map.
   Assocs resolved IDs as :from/:to for the KG edge API."
  [key->id-map edge]
  (let [from-id (get key->id-map (:from-key edge))
        to-id   (get key->id-map (:to-key edge))]
    (when (and from-id to-id)
      (assoc edge :from from-id :to to-id))))

;; =============================================================================
;; Public API
;; =============================================================================

(defn sync-to-kg!
  "Sync LSP analysis operations to hive-mcp KG.

   operations - map from analysis->kg-operations:
     :memory-entries — vec of {:type :content :tags :duration :key}
     :kg-edges       — vec of {:from-key :to-key :relation :confidence :source-type}
     :stats          — {:fns N :edges N :namespaces N}

   Returns {:created N :edges N :errors [str]}."
  [project-id operations scope]
  (let [memory-entries (:memory-entries operations)
        kg-edges       (:kg-edges operations)
        ;; Phase 1: Create memory entries, build key→id mapping
        errors     (volatile! [])
        key->id    (reduce
                    (fn [acc entry]
                      (if-let [id (add-memory-entry! entry project-id)]
                        (assoc acc (:key entry) id)
                        (do (vswap! errors conj (str "Failed entry: " (:key entry)))
                            acc)))
                    {}
                    memory-entries)
        ;; Phase 2: Create KG edges with resolved node IDs
        edge-count (reduce
                    (fn [cnt edge]
                      (if-let [resolved (resolve-edge-node-ids key->id edge)]
                        (if (add-kg-edge! resolved scope)
                          (inc cnt)
                          (do (vswap! errors conj
                                      (str "Failed edge: "
                                           (:from-key edge) " -> " (:to-key edge)))
                              cnt))
                        (do (log/debug "Skipping edge — unresolved nodes:"
                                       (:from-key edge) "->" (:to-key edge))
                            cnt)))
                    0
                    kg-edges)]
    {:created (count key->id)
     :edges   edge-count
     :errors  @errors}))

(defn available?
  "Check if hive-mcp functions are resolvable (Chroma + KG edges)."
  []
  (boolean
   (and (resolve-fn 'hive-mcp.chroma/index-memory-entry!)
        (resolve-fn 'hive-mcp.knowledge-graph.edges/add-edge!))))

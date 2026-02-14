(ns lsp-mcp.transform
  "Pure transformation layer.

   Converts raw LSP analysis data (from analysis.clj) into structures
   compatible with hive-mcp KG/memory. All functions are pure — no side
   effects, no IO, no requiring-resolve.

   Input shapes (from analysis.clj):
     var-defs:   [{:ns sym :name sym :file str :row int :col int
                   :arglists [str] :private? bool :macro? bool :defined-by sym}]
     call-graph: [{:caller-ns sym :caller-fn sym :callee-ns sym :callee-fn sym
                   :file str :row int}]
     ns-graph:   [{:ns sym :depends-on #{sym} :dependents #{sym} :internal? bool}]

   Output shapes:
     memory-entry: {:type \"snippet\" :content str :tags [str] :duration \"medium\"}
     kg-edge:      {:from-key str :to-key str :relation kw
                    :confidence double :source-type kw :created-by str}")

;; =============================================================================
;; Helpers — Memory Entries
;; =============================================================================

(defn var-def->memory-entry
  "Convert a single public var definition to a KG-compatible memory entry.

   Input var-def keys:
     :ns sym, :name sym, :file str, :row int, :col int,
     :arglists [str], :private? bool, :macro? bool, :defined-by sym

   Returns:
     {:type \"snippet\"
      :content \"(defn foo [x y])\\n  Location: src/my/ns.clj:42\"
      :tags [\"lsp\" \"function-def\" \"ns:my.namespace\"]
      :duration \"medium\"
      :key \"ns:my.namespace/foo\"}"
  [_project-id {:keys [ns name file row arglists]}]
  (let [signature (if (seq arglists)
                    (str "(defn " name " " (first arglists) ")")
                    (str "(def " name ")"))
        content   (str signature "\n  Location: " file ":" row)]
    {:type     "snippet"
     :content  content
     :tags     ["lsp" "function-def" (str "ns:" ns)]
     :duration "medium"
     :key      (str "ns:" ns "/" name)}))

(defn namespace->memory-entry
  "Convert namespace graph data to a KG-compatible memory entry.

   Arguments:
     project-id — string project identifier (reserved for future scoping)
     ns-sym     — the namespace symbol
     ns-data    — map from extract-namespace-graph:
                   {:ns sym :depends-on #{sym} :dependents #{sym} :internal? bool}
     var-defs   — full var-defs vector (used to count public vars in this ns)

   Returns:
     {:type \"snippet\"
      :content \"Namespace: my.namespace\\nDependencies: [dep1 dep2]\\nPublic vars: 15\"
      :tags [\"lsp\" \"namespace\" \"ns:my.namespace\"]
      :duration \"medium\"
      :key \"ns:my.namespace\"}"
  [_project-id ns-sym ns-data var-defs]
  (let [deps        (sort (map str (:depends-on ns-data #{})))
        public-vars (->> var-defs
                         (filter #(and (= (str (:ns %)) (str ns-sym))
                                       (not (:private? %))))
                         count)
        content     (str "Namespace: " ns-sym "\n"
                         "Dependencies: " (vec deps) "\n"
                         "Public vars: " public-vars)]
    {:type     "snippet"
     :content  content
     :tags     ["lsp" "namespace" (str "ns:" ns-sym)]
     :duration "medium"
     :key      (str "ns:" ns-sym)}))

;; =============================================================================
;; Helpers — KG Edges
;; =============================================================================

(defn call-edge->kg-edge
  "Convert a call-graph edge to a KG-compatible edge (:depends-on).

   Input call:
     {:caller-ns sym :caller-fn sym :callee-ns sym :callee-fn sym
      :file str :row int}

   Returns:
     {:from-key \"ns:my.ns/foo\" :to-key \"ns:other.ns/bar\"
      :relation :depends-on :confidence 1.0
      :source-type :automated :created-by \"lsp-mcp\"}"
  [{:keys [caller-ns caller-fn callee-ns callee-fn]}]
  {:from-key    (str "ns:" caller-ns "/" caller-fn)
   :to-key      (str "ns:" callee-ns "/" callee-fn)
   :relation    :depends-on
   :confidence  1.0
   :source-type :automated
   :created-by  "lsp-mcp"})

(defn ns-dep->kg-edge
  "Convert a namespace dependency pair to a KG-compatible edge (:depends-on).

   Returns:
     {:from-key \"ns:my.ns\" :to-key \"ns:other.ns\"
      :relation :depends-on :confidence 1.0
      :source-type :automated :created-by \"lsp-mcp\"}"
  [from-ns to-ns]
  {:from-key    (str "ns:" from-ns)
   :to-key      (str "ns:" to-ns)
   :relation    :depends-on
   :confidence  1.0
   :source-type :automated
   :created-by  "lsp-mcp"})

;; =============================================================================
;; Internal — Protocol/Multimethod Implementation Edges
;; =============================================================================

(defn- defmethod?
  "True if var-def was defined via defmethod."
  [var-def]
  (some-> (:defined-by var-def) str (.contains "defmethod")))

(defn- build-defmulti-index
  "Build a lookup {name-string -> {ns-string -> var-def}} for defmulti defs."
  [var-defs]
  (->> var-defs
       (filter #(some-> (:defined-by %) str (.contains "defmulti")))
       (reduce (fn [idx vd]
                 (assoc-in idx [(str (:name vd)) (str (:ns vd))] vd))
               {})))

(defn- impl-edges
  "Create :implements edges for defmethod → defmulti relationships.

   For each defmethod var-def, finds the matching defmulti by name.
   Skips self-edges (defmethod in same ns as defmulti produces identical keys).

   Returns vector of kg-edge maps."
  [var-defs]
  (let [multi-idx (build-defmulti-index var-defs)]
    (->> var-defs
         (filter defmethod?)
         (keep (fn [{:keys [ns name]}]
                 (let [name-str   (str name)
                       candidates (get multi-idx name-str)
                       ;; Prefer defmulti in a different namespace
                       multi-vd   (or (first (vals (dissoc candidates (str ns))))
                                      (get candidates (str ns)))]
                   (when multi-vd
                     (let [from-key (str "ns:" ns "/" name)
                           to-key   (str "ns:" (:ns multi-vd) "/" (:name multi-vd))]
                       ;; Skip self-edges — same ns/name produces identical keys
                       (when (not= from-key to-key)
                         {:from-key    from-key
                          :to-key      to-key
                          :relation    :implements
                          :confidence  1.0
                          :source-type :automated
                          :created-by  "lsp-mcp"}))))))
         vec)))

;; =============================================================================
;; Main Entry Point
;; =============================================================================

(defn analysis->kg-operations
  "Convert LSP analysis data into KG-compatible operations.

   Arguments:
     project-id — string project identifier
     var-defs   — vector of var definition maps (from extract-var-definitions)
     call-graph — vector of call-graph edges   (from extract-call-graph)
     ns-graph   — vector of namespace maps     (from extract-namespace-graph)

   Returns:
     {:memory-entries [...]
      :kg-edges       [...]
      :stats          {:fns N :edges N :namespaces N}}"
  [project-id var-defs call-graph ns-graph]
  (let [;; Memory entries — one per public function/var definition
        public-var-defs (remove :private? var-defs)
        fn-entries      (mapv #(var-def->memory-entry project-id %) public-var-defs)

        ;; Memory entries — one per namespace
        ns-entries (mapv (fn [{:keys [ns] :as ns-data}]
                           (namespace->memory-entry project-id ns ns-data var-defs))
                         ns-graph)

        ;; KG edges — function call dependencies
        call-edges (mapv call-edge->kg-edge call-graph)

        ;; KG edges — namespace-level dependencies
        ns-dep-edges (into []
                           (mapcat (fn [{:keys [ns depends-on]}]
                                     (map #(ns-dep->kg-edge ns %) (or depends-on #{}))))
                           ns-graph)

        ;; KG edges — defmethod :implements defmulti (cross-namespace only)
        implement-edges (impl-edges var-defs)

        ;; Combine all edges
        all-edges (-> call-edges
                      (into ns-dep-edges)
                      (into implement-edges))]

    {:memory-entries (into fn-entries ns-entries)
     :kg-edges       all-edges
     :stats          {:fns        (count fn-entries)
                      :edges      (count all-edges)
                      :namespaces (count ns-entries)}}))

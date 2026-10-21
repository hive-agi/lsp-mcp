(ns lsp-mcp.bridge
  "Protocol for live LSP queries — the stable abstraction — and the open
   registry live bridges install themselves into.

   lsp-mcp ships no live bridge and names no vessel. A vessel that can answer
   live LSP queries (hive-emacs over lsp-mode, a direct clojure-lsp stdio
   client, ...) registers one under its own key with `register-bridge!`,
   usually built with `fn-bridge` so it needs no compile-time dependency on
   this protocol. The tools ask `active-bridge` on every call, so a vessel
   that starts after lsp-mcp is picked up without a restart.")

(defprotocol ILspBridge
  "Live LSP query interface. All methods return Clojure data or {:error \"...\"}."
  (bridge-available? [this]
    "Is this bridge implementation currently reachable?")
  (bridge-status [this]
    "Bridge status: {:lsp-available bool, :workspace-count n, :workspaces [...]}")
  (bridge-workspaces [this]
    "List active workspaces: [{:root str, :server-id str, ...}]")
  (bridge-hover [this project-root file-path line column]
    "textDocument/hover at position. Returns LSP Hover or {:error ...}")
  (bridge-definition [this project-root file-path line column]
    "textDocument/definition at position. Returns Location(s) or {:error ...}")
  (bridge-references [this project-root file-path line column]
    "textDocument/references at position. Returns [Location] or {:error ...}")
  (bridge-document-symbols [this project-root file-path]
    "textDocument/documentSymbol for file. Returns [DocumentSymbol] or {:error ...}")
  (bridge-cursor-info [this project-root file-path line column]
    "clojure/cursorInfo/raw at position. Returns cursor data or {:error ...}")
  (bridge-server-info [this project-root]
    "clojure/serverInfo/raw for project. Returns server data or {:error ...}"))

;; =============================================================================
;; Registry
;; =============================================================================

(defonce ^:private bridges (atom {}))

(defn register-bridge!
  "Install BRIDGE (an ILspBridge) under KEY, replacing any previous one.
   Returns BRIDGE."
  [key bridge]
  (when-not (satisfies? ILspBridge bridge)
    (throw (ex-info "register-bridge! needs an ILspBridge" {:key key})))
  (swap! bridges assoc key bridge)
  bridge)

(defn unregister-bridge!
  "Remove the bridge under KEY. No-op when absent. Returns nil."
  [key]
  (swap! bridges dissoc key)
  nil)

(defn registered-bridges
  "A read-only {key -> bridge} snapshot."
  []
  @bridges)

(defn active-bridge
  "The first registered bridge (by key) that reports itself available, or
   nil. A bridge whose availability check throws counts as unavailable."
  []
  (some (fn [[_ b]] (when (try (bridge-available? b) (catch Exception _ false)) b))
        (sort-by (comp str key) @bridges)))

(defn fn-bridge
  "An ILspBridge over plain fns, for a vessel that should not compile against
   this protocol.

     :available?  (fn [] bool)
     :call        (fn [method & args]) -> Clojure data or {:error \"...\"};
                  method is one of \"status\" \"workspaces\" \"hover\"
                  \"definition\" \"references\" \"document-symbols\"
                  \"cursor-info\" \"server-info\", args as in the protocol."
  [{:keys [available? call]}]
  (reify ILspBridge
    (bridge-available? [_] (boolean (available?)))
    (bridge-status [_] (call "status"))
    (bridge-workspaces [_] (call "workspaces"))
    (bridge-hover [_ root file line col] (call "hover" root file line col))
    (bridge-definition [_ root file line col] (call "definition" root file line col))
    (bridge-references [_ root file line col] (call "references" root file line col))
    (bridge-document-symbols [_ root file] (call "document-symbols" root file))
    (bridge-cursor-info [_ root file line col] (call "cursor-info" root file line col))
    (bridge-server-info [_ root] (call "server-info" root))))

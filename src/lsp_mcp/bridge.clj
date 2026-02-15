(ns lsp-mcp.bridge
  "Protocol for live LSP queries — the stable abstraction.

   Implementations:
   - lsp-mcp.emacs-bridge (Strategy 3: Emacs lsp-mode via emacsclient)
   - Future: direct clojure-lsp stdio, LSP over TCP, etc.")

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

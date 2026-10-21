# lsp-mcp

<!-- hive-badges -->

[![Clojars Project](https://img.shields.io/clojars/v/io.github.hive-agi/lsp-mcp.svg)](https://clojars.org/io.github.hive-agi/lsp-mcp)
[![cljdoc](https://cljdoc.org/badge/io.github.hive-agi/lsp-mcp)](https://cljdoc.org/d/io.github.hive-agi/lsp-mcp/CURRENT)
[![release](https://github.com/hive-agi/lsp-mcp/actions/workflows/release.yml/badge.svg)](https://github.com/hive-agi/lsp-mcp/actions/workflows/release.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- /hive-badges -->

Clojure LSP analysis addon for hive-mcp. Provides structural code analysis via three strategies:

1. **Docker sidecar cache** -- fast reads from pre-analyzed snapshots
2. **In-process clojure-lsp** -- fallback when cache is unavailable
3. **Live LSP bridge** -- real-time, position-aware queries through whichever bridge a vessel has registered

Registered as an IAddon (`lsp.mcp`). The engine discovers it automatically from the classpath manifest.

lsp-mcp names no vessel. It ships no editor code and starts no editor.

## Project Structure

```
lsp-mcp/
  src/lsp_mcp/
    bridge.clj          # ILspBridge protocol + the live-bridge registry
    tools.clj            # MCP tool handlers + command dispatch
    init.clj             # IAddon implementation + nil-railway pipeline
    core.clj             # Analysis orchestration
    cache.clj            # Docker sidecar cache client
    analysis.clj         # Extraction helpers (vars, calls, ns-graph)
    kg_bridge.clj        # Knowledge Graph sync bridge
    log.clj              # Logging shim (timbre/stderr)
  resources/
    META-INF/hive-addons/
      lsp-mcp.edn          # Addon manifest
```

## Live bridges

A live bridge is any `lsp-mcp.bridge/ILspBridge`. A vessel registers one under its own key:

```clojure
;; soft-resolved, so the vessel does not compile against lsp-mcp
((requiring-resolve 'lsp-mcp.bridge/register-bridge!)
 :my-vessel
 ((requiring-resolve 'lsp-mcp.bridge/fn-bridge)
  {:available? (fn [] true)
   :call       (fn [method & args] ...)}))   ; method: "hover", "definition", ...
```

The tools ask `active-bridge` on every call, so a vessel that starts after lsp-mcp is picked up without a restart. With no bridge registered, the live commands answer `:bridge/unavailable`.

Known bridges:

| Bridge | Lives in | Backend |
|---|---|---|
| `:emacs` | hive-emacs (`hive-emacs.lsp-bridge`, `elisp/hive-lsp-bridge.el`) | lsp-mode + clojure-lsp in a running Emacs |

## MCP Commands

### Static analysis (strategies 1 & 2)

| Command | Description |
|---|---|
| `analyze` | Project summary (files, namespaces, vars) |
| `definitions` | List var definitions, optionally filtered by namespace |
| `calls` | Call graph, filtered by namespace/function |
| `callers` | Find callers of a function |
| `references` | Find references to a function |
| `ns-graph` | Namespace dependency graph |
| `sync` | Sync analysis to Knowledge Graph |
| `status` | Cache and bridge status |

### Live LSP bridge (strategy 3)

| Command | Parameters | Description |
|---|---|---|
| `bridge-status` | -- | Check bridge availability and workspace count |
| `workspaces` | -- | List active LSP workspaces |
| `hover` | `project_root`, `file_path`, `line`, `column` | Hover info at position |
| `definition` | `project_root`, `file_path`, `line`, `column` | Go-to-definition |
| `live-references` | `project_root`, `file_path`, `line`, `column` | Find all references |
| `symbols` | `project_root`, `file_path` | Document symbols |
| `cursor-info` | `project_root`, `file_path`, `line`, `column` | clojure-lsp cursor info |
| `server-info` | `project_root` | clojure-lsp server info |

`line` and `column` are 0-based.

## Architecture

```
MCP tool call
  -> tools.clj (command dispatch)
       |
       +-- Static: core.clj -> cache.clj / clojure-lsp.api
       |
       +-- Live:   bridge/active-bridge -> ILspBridge
                     |
                     +-- whatever a vessel registered (e.g. hive-emacs :emacs)
```

The `ILspBridge` protocol (DIP) decouples `tools.clj` from any LSP backend; the registry decouples lsp-mcp from any vessel.

## License

MIT

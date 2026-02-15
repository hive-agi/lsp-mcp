# lsp-mcp

Clojure LSP analysis addon for hive-mcp. Provides structural code analysis via three strategies:

1. **Docker sidecar cache** -- fast reads from pre-analyzed snapshots
2. **In-process clojure-lsp** -- fallback when cache is unavailable
3. **Live Emacs LSP bridge** -- real-time, position-aware queries via running clojure-lsp in Emacs

Registered as an IAddon (`lsp.mcp`). The engine discovers it automatically from the classpath manifest.

## Project Structure

```
lsp-mcp/
  src/lsp_mcp/
    bridge.clj          # ILspBridge protocol (DIP abstraction)
    emacs_bridge.clj     # EmacsBridge -- Emacs backend (optional)
    tools.clj            # MCP tool handlers + command dispatch
    init.clj             # IAddon implementation + nil-railway pipeline
    core.clj             # Analysis orchestration
    cache.clj            # Docker sidecar cache client
    analysis.clj         # Extraction helpers (vars, calls, ns-graph)
    kg_bridge.clj        # Knowledge Graph sync bridge
    log.clj              # Logging shim (timbre/stderr)
  src-cljel/
    hive_lsp_bridge.cljel  # Elisp source (Clojure syntax)
  resources/
    elisp/
      hive-lsp-bridge.el   # Compiled output (checked in)
    META-INF/hive-addons/
      lsp-mcp.edn          # Addon manifest
```

## Compiling `.cljel` to `.el`

The `src-cljel/` directory contains Elisp source written in Clojure syntax using [clojure-elisp](https://github.com/lages/clojure-elisp). The compiler translates `.cljel` files to standard Emacs Lisp `.el` files.

### One-time compilation

From the lsp-mcp project root:

```bash
clojure \
  -Sdeps '{:deps {io.github.lages/clojure-elisp {:local/root "/home/lages/PP/clojure-elisp"}}}' \
  -M -e '(require (quote [clojure-elisp.core :as clel]))
         (clel/compile-file "src-cljel/hive_lsp_bridge.cljel"
                            "resources/elisp/hive-lsp-bridge.el")'
```

This reads the `.cljel` source, compiles all forms through the clojure-elisp analyzer/emitter, and writes the output `.el` file. The compiled file:

- Starts with `;;; hive-lsp-bridge.el --- -*- lexical-binding: t; -*-`
- Requires `clojure-elisp-runtime` (must be on Emacs `load-path`)
- Ends with `(provide 'hive-lsp-bridge)`

### Naming conventions

| `.cljel` source | compiled `.el` output | Elisp feature name |
|---|---|---|
| `hive_lsp_bridge.cljel` | `hive-lsp-bridge.el` | `hive-lsp-bridge` |

The compiler converts underscores to hyphens and derives the feature name from the `(ns ...)` form.

### Function name mapping

| `.cljel` definition | compiled Elisp name |
|---|---|
| `(defn status ...)` | `hive-lsp-bridge-status` |
| `(defn -find-workspace ...)` | `hive-lsp-bridge--find-workspace` |
| `(defn hover ...)` | `hive-lsp-bridge-hover` |

Public functions get the namespace prefix (`hive-lsp-bridge-`). Private functions (prefixed with `-`) get a double-dash (`hive-lsp-bridge--`).

### Live development with CIDER

For interactive development without recompiling the whole file:

1. Open `hive_lsp_bridge.cljel` in Emacs
2. `cider-jack-in` to the clojure-elisp project
3. `M-x cider-cljel-start` to enable the cljel CIDER session
4. `C-c C-e` on any `defn` form -- compiles to Elisp and evals in Emacs immediately
5. `M-: (hive-lsp-bridge-status)` to verify the function works

### When to recompile

Recompile and commit the `.el` output after changing any `.cljel` source. The compiled `.el` is checked into version control so that Emacs can load it at runtime without needing the compiler on the classpath.

## Runtime: how the `.el` gets loaded

The compiled `.el` is loaded **on-the-fly** by the Clojure bridge, not by user Emacs config. The sequence:

1. MCP tool call arrives (e.g. `lsp command=emacs-hover`)
2. `emacs_bridge.clj` injects `resources/elisp/` into Emacs `load-path` (once)
3. `require-and-call-json` generates: `(progn (require 'hive-lsp-bridge nil t) (json-encode (hive-lsp-bridge-hover ...)))`
4. `eval-elisp-with-timeout` sends it to Emacs via `emacsclient --eval`
5. Emacs loads the feature, calls the function, returns JSON

Prerequisites:
- `clojure-elisp-runtime` must be on Emacs `load-path` (from the clojure-elisp project)
- Emacs daemon running with `lsp-mode` and `clojure-lsp` active

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

### Live Emacs LSP bridge (strategy 3)

| Command | Parameters | Description |
|---|---|---|
| `emacs-status` | -- | Check bridge availability and workspace count |
| `emacs-workspaces` | -- | List active LSP workspaces |
| `emacs-hover` | `project_root`, `file_path`, `line`, `column` | Hover info at position |
| `emacs-definition` | `project_root`, `file_path`, `line`, `column` | Go-to-definition |
| `emacs-references` | `project_root`, `file_path`, `line`, `column` | Find all references |
| `emacs-symbols` | `project_root`, `file_path` | Document symbols |
| `emacs-cursor-info` | `project_root`, `file_path`, `line`, `column` | clojure-lsp cursor info |
| `emacs-server-info` | `project_root` | clojure-lsp server info |

`line` and `column` are 0-based.

## Architecture

```
MCP tool call
  -> tools.clj (command dispatch)
       |
       +-- Static: core.clj -> cache.clj / clojure-lsp.api
       |
       +-- Live:   ILspBridge protocol
                     |
                     +-- EmacsBridge (emacs_bridge.clj)
                           |
                           +-- emacsclient --eval
                                 |
                                 +-- (require 'hive-lsp-bridge)  [compiled .el]
                                       |
                                       +-- lsp-request -> clojure-lsp in Emacs
```

The `ILspBridge` protocol (DIP) decouples `tools.clj` from any specific LSP backend. `EmacsBridge` is one implementation; a future `StdioBridge` could talk to clojure-lsp directly over stdio.

## License

MIT

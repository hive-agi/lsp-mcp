(ns lsp-mcp.deps
  "Pure deps.edn inspection — no IO.

   Built for the sidecar pre-flight: the LSP sidecar runs inside Docker
   with a single workspace mount, so any `:local/root` declared in
   deps.edn that resolves OUTSIDE that mount cannot be seen by
   clojure-lsp. We surface those before triggering the sidecar so the
   caller gets an ELM-style error instead of a 60s timeout."
  (:require [babashka.fs :as bfs]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn parse-deps-edn
  "Parse a deps.edn string into a Clojure map. Returns nil on parse
   failure — caller decides how to surface the error."
  [^String content]
  (try
    (edn/read-string content)
    (catch Exception _ nil)))

(defn- coll-of-deps
  "Return seq of dep-maps from a deps map: top-level :deps + every
   :extra-deps under :aliases. Each yielded entry is a [coord-symbol
   coord-map] tuple, since `:local/root` lives on the coord-map."
  [deps-edn]
  (concat (seq (:deps deps-edn))
          (mapcat (fn [[_alias-key alias-map]]
                    (seq (:extra-deps alias-map)))
                  (:aliases deps-edn))))

(defn local-root-deps
  "Return a vector of {:dep coord-symbol :path string} entries for
   every coordinate (top-level or alias-scoped) that uses `:local/root`.

   The path is returned verbatim — caller is responsible for
   canonicalizing relative to the project root."
  [deps-edn]
  (into []
        (keep (fn [[coord-sym coord-map]]
                (when-let [root (:local/root coord-map)]
                  {:dep coord-sym :path root})))
        (coll-of-deps deps-edn)))

(defn- canonicalize
  "Canonicalize `path` relative to `base-dir`. Returns absolute path
   string. Pure-ish: bfs/canonicalize is a static path computation —
   it does NOT require the path to exist."
  [base-dir path]
  (let [^java.io.File f (java.io.File. ^String path)
        absolute (if (.isAbsolute f)
                   f
                   (java.io.File. ^java.io.File (java.io.File. ^String base-dir)
                                  ^String path))]
    (str (bfs/normalize (bfs/absolutize (.toPath absolute))))))

(defn unreachable-roots
  "Partition `local-roots` against `workspace-root`. A root is
   reachable iff its canonicalized absolute path lives under the
   workspace root.

   Args:
     local-roots     vec of {:dep :path} (output of local-root-deps).
     project-root    absolute path of the project containing deps.edn
                     (used as base for relative :local/root values).
     workspace-root  absolute path of the sidecar workspace mount.

   Returns: vec of {:dep :path :resolved} for entries that are
   UNREACHABLE. Reachable entries are dropped. Order is preserved."
  [local-roots project-root workspace-root]
  (let [ws (str (bfs/normalize (bfs/absolutize (bfs/path workspace-root))))
        ws-prefix (if (str/ends-with? ws "/") ws (str ws "/"))]
    (into []
          (keep (fn [{:keys [dep path] :as entry}]
                  (let [resolved (canonicalize project-root path)]
                    (when-not (or (= resolved ws)
                                  (str/starts-with? resolved ws-prefix))
                      (assoc entry :resolved resolved)))))
          local-roots)))

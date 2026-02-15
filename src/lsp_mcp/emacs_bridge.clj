(ns lsp-mcp.emacs-bridge
  "ILspBridge implementation: Emacs lsp-mode via emacsclient.

   Optional backend — Emacs need not be running. The .el feature is
   loaded on-the-fly via require-and-call-json, no user Emacs config needed.

   hive-mcp.emacs.{client,elisp} resolved lazily via requiring-resolve
   so lsp-mcp works standalone without hive-mcp on classpath."
  (:require [lsp-mcp.bridge :as bridge]
            [lsp-mcp.log :as log]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; =============================================================================
;; Lazy Resolution (optional deps — hive-mcp may not be on classpath)
;; =============================================================================

(defn- resolve-eval-fn
  "Resolve hive-mcp.emacs.client/eval-elisp-with-timeout.
   Returns the fn or nil if hive-mcp is not available."
  []
  (try
    (requiring-resolve 'hive-mcp.emacs.client/eval-elisp-with-timeout)
    (catch Exception e
      (log/debug "hive-mcp.emacs.client not available:" (.getMessage e))
      nil)))

(defn- resolve-elisp-fn
  "Resolve hive-mcp.emacs.elisp/require-and-call-json.
   Returns the fn or nil if hive-mcp is not available."
  []
  (try
    (requiring-resolve 'hive-mcp.emacs.elisp/require-and-call-json)
    (catch Exception e
      (log/debug "hive-mcp.emacs.elisp not available:" (.getMessage e))
      nil)))

;; =============================================================================
;; Load-Path Injection (one-time, on first use)
;; =============================================================================

(defonce ^:private load-path-injected?
  (delay
    (when-let [eval-fn (resolve-eval-fn)]
      (when-let [res-url (io/resource "elisp/hive-lsp-bridge.el")]
        (let [elisp-dir (-> (.getPath res-url)
                            (str/replace #"/hive-lsp-bridge\.el$" ""))]
          (log/debug "Injecting load-path:" elisp-dir)
          (let [result (eval-fn
                        (format "(add-to-list 'load-path \"%s\")" elisp-dir)
                        5000)]
            (if (:success result)
              (do (log/info "Emacs LSP bridge load-path injected") true)
              (do (log/warn "Failed to inject load-path:" (:error result)) false))))))))

;; =============================================================================
;; Core Bridge Call
;; =============================================================================

(def ^:private feature 'hive-lsp-bridge)

(defn- lsp-call
  "Call a hive-lsp-bridge function via emacsclient.
   Injects load-path on first use, then uses require-and-call-json.
   Returns parsed Clojure data or {:error \"...\"}."
  [fn-name & args]
  (let [eval-fn  (resolve-eval-fn)
        elisp-fn (resolve-elisp-fn)]
    (if (and eval-fn elisp-fn)
      (do
        @load-path-injected?
        (let [elisp (apply elisp-fn feature (symbol fn-name) args)
              result (eval-fn elisp 10000)]
          (if (:success result)
            (try
              (json/read-str (:result result) :key-fn keyword)
              (catch Exception e
                (log/warn "JSON parse failed for" fn-name ":" (.getMessage e))
                {:error (str "JSON parse error: " (.getMessage e))
                 :raw   (:result result)}))
            {:error (or (:error result) "emacsclient call failed")})))
      {:error "hive-mcp emacs bridge not available (not on classpath)"})))

;; =============================================================================
;; ILspBridge Implementation
;; =============================================================================

(defrecord EmacsBridge []
  bridge/ILspBridge
  (bridge-available? [_]
    (boolean (and (resolve-eval-fn) (resolve-elisp-fn))))
  (bridge-status [_]
    (lsp-call "hive-lsp-bridge-status"))
  (bridge-workspaces [_]
    (lsp-call "hive-lsp-bridge-workspaces"))
  (bridge-hover [_ project-root file-path line column]
    (lsp-call "hive-lsp-bridge-hover" project-root file-path line column))
  (bridge-definition [_ project-root file-path line column]
    (lsp-call "hive-lsp-bridge-definition" project-root file-path line column))
  (bridge-references [_ project-root file-path line column]
    (lsp-call "hive-lsp-bridge-references" project-root file-path line column))
  (bridge-document-symbols [_ project-root file-path]
    (lsp-call "hive-lsp-bridge-document-symbols" project-root file-path))
  (bridge-cursor-info [_ project-root file-path line column]
    (lsp-call "hive-lsp-bridge-cursor-info" project-root file-path line column))
  (bridge-server-info [_ project-root]
    (lsp-call "hive-lsp-bridge-server-info" project-root)))

(defn make-emacs-bridge
  "Create an EmacsBridge instance."
  []
  (->EmacsBridge))

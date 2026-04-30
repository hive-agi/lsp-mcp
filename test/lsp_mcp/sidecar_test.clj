(ns lsp-mcp.sidecar-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [lsp-mcp.cache :as cache]
            [lsp-mcp.sidecar :as sidecar]))

;; -----------------------------------------------------------------------------
;; ensure-analysis! — unresolvable project-id guard
;;
;; The sidecar resolves a request as <workspace-root>/<project-id> on
;; the host. If that path is not a real directory, clojure-lsp inside
;; the container has nothing to dump, the cache never populates, and
;; await-cache-ready burns the full 60s timeout. We fail fast instead.

(deftest ensure-analysis!-unresolvable-project-id-fails-fast
  (testing "bogus project-id returns ELM error without invoking sidecar"
    (with-redefs [cache/read-analysis (fn [_] nil)
                  cache/workspace-root (fn [] "/strictly-isolated-ws")]
      (let [result (sidecar/ensure-analysis! "/path/to/project")]
        (is (= :analysis/unresolvable-project-id (:error result)))
        (is (= :pass-resolvable-id (:fix result)))
        (is (string? (:hint result)))
        (is (str/includes? (:hint result) "I could not resolve"))
        (is (str/includes? (:hint result) "/path/to/project"))
        (is (str/includes? (:hint result) "Resolution attempts"))
        (is (str/includes? (:hint result) "Valid examples"))
        (is (str/includes? (:hint result) "/strictly-isolated-ws"))))))

(deftest ensure-analysis!-cache-hit-bypasses-validation
  (testing "even bogus project-ids return ok when cache is hot"
    (with-redefs [cache/read-analysis (fn [_] {:cached true})]
      (is (= {:ok {:cached true}}
             (sidecar/ensure-analysis! "/path/to/project"))))))

(deftest ensure-analysis!-workspace-sentinel-resolves
  (testing "literal 'workspace' is allowed (matches cache/project-id-for)"
    (let [seen (atom nil)]
      (with-redefs [cache/read-analysis (fn [_] nil)
                    cache/workspace-root (fn [] "/strictly-isolated-ws")
                    sidecar/ensure-sidecar-running! (fn []
                                                      (reset! seen :sidecar-checked)
                                                      {:error :analysis/sidecar-unavailable
                                                       :message "stop here"})]
        (sidecar/ensure-analysis! "workspace")
        ;; Without the sentinel exception this would short-circuit on
        ;; the unresolvable guard and never touch the sidecar layer.
        (is (= :sidecar-checked @seen))))))

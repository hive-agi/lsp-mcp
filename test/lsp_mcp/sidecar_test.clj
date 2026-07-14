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

;; -----------------------------------------------------------------------------
;; refresh-analysis! — forces a FRESH dump, defeating the present-but-stale cache
;;
;; ensure-analysis!/await-cache-ready both return on any present cache (even
;; stale), so neither re-dumps after a config change. refresh-analysis! must
;; poll until the on-disk meta timestamp ADVANCES, then invalidate the in-mem
;; parse. The "workspace" sentinel resolves without a real host dir.

(deftest refresh-analysis!-waits-for-timestamp-advance
  (testing "polls until meta generation advances, then invalidates the in-mem cache"
    (let [metas       (atom [{:timestamp 100 :status :ok}
                             {:timestamp 200 :status :ok}])
          invalidated (atom nil)]
      (with-redefs [cache/read-meta (fn [_]
                                      (let [m (first @metas)]
                                        (when (next @metas) (swap! metas rest))
                                        m))
                    cache/read-analysis (fn [_ _] {:fresh true})
                    sidecar/ensure-sidecar-running! (fn [] {:ok true})
                    sidecar/request-analysis!       (fn [_] {:requested true})
                    cache/invalidate!               (fn [pid] (reset! invalidated pid))]
        (let [r (sidecar/refresh-analysis! "workspace")]
          (is (:refreshed? r))
          (is (= 100 (:old-ts r)))
          (is (= 200 (:new-ts r)))
          (is (= "workspace" @invalidated)
              "in-mem parse invalidated for the project"))))))

(deftest await-cache-ready-surfaces-terminal-dump-error
  (testing "a fresh error generation returns immediately instead of timing out"
    (with-redefs [cache/read-meta (fn [_]
                                   {:timestamp 101
                                    :completed-at-ms 101000
                                    :status :error
                                    :exit-code 17})
                  cache/cache-dir (fn [] "/tmp/hive-lsp-test")]
      (let [result (sidecar/await-cache-ready
                    "hive/broken"
                    {:timestamp 100 :status :ok}
                    100)]
        (is (= :analysis/sidecar-dump-failed (:error result)))
        (is (= 17 (:exit-code result)))
        (is (str/includes? (:log-path result) "hive/broken/dump.log"))))))

(deftest await-cache-ready-reads-only-new-success-generation
  (testing "a new ok generation invalidates stale parsed data before reading"
    (let [invalidated (atom nil)]
      (with-redefs [cache/read-meta (fn [_]
                                     {:timestamp 101
                                      :completed-at-ms 101000
                                      :status :ok})
                    cache/invalidate! (fn [pid] (reset! invalidated pid))
                    cache/read-analysis (fn [_ _] {:fresh true})]
        (let [result (sidecar/await-cache-ready
                      "hive/ready"
                      {:timestamp 100 :status :ok}
                      100)]
          (is (= {:ok {:fresh true}} result))
          (is (= "hive/ready" @invalidated)))))))

(deftest refresh-analysis!-propagates-sidecar-unavailable
  (testing "returns the ensure-sidecar-running! error without polling"
    (with-redefs [cache/read-meta (fn [_] {:timestamp 100 :status :ok})
                  sidecar/ensure-sidecar-running! (fn [] {:error :analysis/sidecar-unavailable
                                                          :message "down"})]
      (let [r (sidecar/refresh-analysis! "workspace")]
        (is (= :analysis/sidecar-unavailable (:error r)))))))

(deftest refresh-analysis!-fails-fast-on-unresolvable-id
  (testing "bogus project-id returns ELM error without touching the sidecar"
    (with-redefs [cache/workspace-root (fn [] "/strictly-isolated-ws")]
      (let [r (sidecar/refresh-analysis! "/no/such/project")]
        (is (= :analysis/unresolvable-project-id (:error r)))))))

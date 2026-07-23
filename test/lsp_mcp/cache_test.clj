(ns lsp-mcp.cache-test
  (:require [clojure.test :refer [deftest is testing]]
            [lsp-mcp.cache :as cache]
            [clojure.java.io :as io]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn- create-temp-cache
  "Create a temporary cache directory. Returns the temp dir path string."
  []
  (let [dir (java.io.File/createTempFile "lsp-cache-test" "")]
    (.delete dir)
    (.mkdirs dir)
    (.getAbsolutePath dir)))

(defn- write-cache-files!
  "Write cache files for a project in the temp cache dir."
  [cache-dir project-id {:keys [dump-data meta-data]}]
  (let [project-dir (io/file cache-dir project-id)]
    (.mkdirs project-dir)
    (when dump-data
      (spit (io/file project-dir "dump.edn") (pr-str dump-data)))
    (when meta-data
      (spit (io/file project-dir "meta.edn") (pr-str meta-data)))))

(defn- current-epoch-seconds []
  (quot (System/currentTimeMillis) 1000))

(defn- with-test-cache
  "Execute f with cache-dir overridden to a temp directory.
   f receives the temp-dir path as argument. Cleans up after."
  [f]
  (let [temp-dir (create-temp-cache)]
    (try
      (with-redefs [cache/cache-dir (constantly temp-dir)]
        (f temp-dir))
      (finally
        (doseq [file (reverse (file-seq (io/file temp-dir)))]
          (.delete ^java.io.File file))))))

;; =============================================================================
;; Sample Data
;; =============================================================================

(def ^:private sample-dump
  {:analysis {"file:///src/test/ns.clj"
              {:var-definitions [{:ns   'test.ns
                                  :name 'foo
                                  :row  1
                                  :col  1}]}}
   :dep-graph {'test.ns {:dependencies {'clojure.core 1}
                         :dependents   {}
                         :internal?    true}}})

(def ^:private fresh-meta
  {:timestamp   (current-epoch-seconds)
   :duration-ms 1500
   :project-id  "test-project"
   :status      :ok})

(def ^:private stale-meta
  {:timestamp   0
   :duration-ms 2000
   :project-id  "test-project"
   :status      :ok})

(def ^:private error-meta
  {:timestamp   (current-epoch-seconds)
   :duration-ms 500
   :project-id  "test-project"
   :status      :error
   :exit-code   1})

;; =============================================================================
;; Tests
;; =============================================================================

(deftest test-read-analysis-cache-hit
  (testing "returns cached analysis when fresh and valid"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "test-project"
                            {:dump-data sample-dump
                             :meta-data fresh-meta})
        (let [result (cache/read-analysis "test-project")]
          (is (some? result))
          (is (contains? result :analysis))
          (is (contains? result :dep-graph)))))))

(deftest test-read-analysis-rebases-sidecar-source-roots
  (testing "focused cache file URIs resolve through the host workspace mount"
    (with-test-cache
      (fn [temp-dir]
        (let [project-id "rebase-project"
              project-dir (io/file temp-dir project-id)]
          (.mkdirs project-dir)
          (spit (io/file project-dir "meta.edn")
                (pr-str (assoc fresh-meta :project-id project-id)))
          (spit (io/file project-dir "var-defs.edn")
                (pr-str [{:file "file:///workspace/hive/rebase/src/a.clj"}]))
          (spit (io/file project-dir "call-graph.edn")
                (pr-str [{:file "file:/workspace/hive/rebase/src/a.clj"}]))
          (spit (io/file project-dir "ns-graph.edn") "{}")
          (spit (io/file project-dir "ns-defs.edn") "[]")
          (spit (io/file project-dir "summary.edn") "{}")
          (with-redefs [cache/workspace-root (constantly "/host/PP")]
            (let [result (cache/read-analysis project-id)]
              (is (= "file:///host/PP/hive/rebase/src/a.clj"
                     (:file (first (:var-defs result)))))
              (is (= "file:///host/PP/hive/rebase/src/a.clj"
                     (:file (first (:call-graph result))))))))))))

(deftest test-read-analysis-no-cache
  (testing "returns nil for non-existent project"
    (with-test-cache
      (fn [_temp-dir]
        (is (nil? (cache/read-analysis "non-existent-project")))))))

(deftest test-read-analysis-stale
  (testing "returns nil when cache is stale"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "test-project"
                            {:dump-data sample-dump
                             :meta-data stale-meta})
        (is (nil? (cache/read-analysis "test-project")))))))

(deftest test-read-analysis-ignore-staleness
  (testing "returns stale data when :ignore-staleness true"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "test-project"
                            {:dump-data sample-dump
                             :meta-data stale-meta})
        (let [result (cache/read-analysis "test-project"
                                          {:ignore-staleness true})]
          (is (some? result))
          (is (contains? result :analysis)))))))

(deftest test-read-analysis-error-status
  (testing "returns nil when cache status is :error"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "test-project"
                            {:dump-data sample-dump
                             :meta-data error-meta})
        (is (nil? (cache/read-analysis "test-project")))))))

(deftest test-cache-fresh?
  (testing "fresh cache returns true"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "test-project"
                            {:meta-data fresh-meta})
        (is (true? (cache/cache-fresh? "test-project"))))))
  (testing "stale cache returns falsy"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "test-project"
                            {:meta-data stale-meta})
        (is (not (cache/cache-fresh? "test-project"))))))
  (testing "missing project returns nil"
    (with-test-cache
      (fn [_temp-dir]
        (is (nil? (cache/cache-fresh? "missing")))))))

(deftest test-list-cached-projects
  (testing "lists all projects with meta.edn"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "project-a"
                            {:meta-data fresh-meta})
        (write-cache-files! temp-dir "project-b"
                            {:meta-data fresh-meta})
        ;; Directory without meta.edn should not appear
        (.mkdirs (io/file temp-dir "project-c"))
        (let [projects (set (cache/list-cached-projects))]
          (is (contains? projects "project-a"))
          (is (contains? projects "project-b"))
          (is (not (contains? projects "project-c"))))))))

(deftest test-list-cached-projects-recursive
  (testing "finds nested and job-only projects while pruning kondo-cache"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "hive/lsp-mcp"
                            {:meta-data fresh-meta})
        (let [queued-dir (io/file temp-dir "hive/queued")
              ignored-dir (io/file temp-dir "kondo-cache/library")]
          (.mkdirs queued-dir)
          (.mkdirs ignored-dir)
          (spit (io/file queued-dir "job.edn")
                (pr-str {:status :queued}))
          (spit (io/file ignored-dir "meta.edn")
                (pr-str fresh-meta))
          (is (= ["hive/lsp-mcp" "hive/queued"]
                 (cache/list-cached-projects))))))))

(deftest test-cache-status
  (testing "returns status map with project details"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "test-project"
                            {:dump-data sample-dump
                             :meta-data fresh-meta})
        (let [status (cache/cache-status)]
          (is (= temp-dir (:cache-dir status)))
          (is (= 1 (count (:projects status))))
          (let [proj (first (:projects status))]
            (is (= "test-project" (:project-id proj)))
            (is (= :ok (:status proj)))
            (is (true? (:fresh? proj)))
            (is (number? (:duration-ms proj)))))))))

(deftest test-cache-status-includes-job-and-queue-telemetry
  (testing "queued jobs and the worker heartbeat remain visible before completion"
    (with-test-cache
      (fn [temp-dir]
        (let [project-id "hive/queued"
              queued-at (- (System/currentTimeMillis) 50)]
          (write-cache-files! temp-dir project-id
                              {:meta-data (assoc fresh-meta :project-id project-id)})
          (spit (io/file temp-dir project-id "job.edn")
                (pr-str {:job-id "job-1"
                         :project-id project-id
                         :status :queued
                         :queued-at-ms queued-at}))
          (spit (io/file temp-dir "_health.edn")
                (pr-str {:status :ok :heartbeat-at-ms queued-at}))
          (let [status (cache/cache-status)
                project (first (:projects status))]
            (is (= :queued (:status project)))
            (is (= :ok (:cache-status project)))
            (is (= "job-1" (get-in project [:job :job-id])))
            (is (= 1 (get-in status [:queue :queued])))
            (is (pos? (get-in status [:queue :oldest-queued-ms])))
            (is (= :ok (get-in status [:sidecar-heartbeat :status])))))))))

(deftest test-invalidate!-forces-reparse
  (testing "invalidate! drops the in-memory parse so the next read re-reads disk"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "inv-proj"
                            {:dump-data {:marker :v1} :meta-data fresh-meta})
        (is (= :v1 (:marker (cache/read-analysis "inv-proj")))
            "first read parses v1 from disk and caches it in-memory")
        ;; Overwrite dump.edn WITHOUT bumping the meta timestamp → the in-memory
        ;; parse (keyed by timestamp) still serves the stale v1.
        (spit (io/file temp-dir "inv-proj" "dump.edn") (pr-str {:marker :v2}))
        (is (= :v1 (:marker (cache/read-analysis "inv-proj")))
            "stale in-memory parse still served while meta timestamp unchanged")
        (cache/invalidate! "inv-proj")
        (is (= :v2 (:marker (cache/read-analysis "inv-proj")))
            "after invalidate! the disk is re-parsed → v2")))))

(deftest test-read-meta
  (testing "reads meta.edn correctly"
    (with-test-cache
      (fn [temp-dir]
        (write-cache-files! temp-dir "test-project"
                            {:meta-data fresh-meta})
        (let [meta (cache/read-meta "test-project")]
          (is (= :ok (:status meta)))
          (is (number? (:timestamp meta)))
          (is (= "test-project" (:project-id meta)))))))
  (testing "returns nil for missing project"
    (with-test-cache
      (fn [_temp-dir]
        (is (nil? (cache/read-meta "missing")))))))
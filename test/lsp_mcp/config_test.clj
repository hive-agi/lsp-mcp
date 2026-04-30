(ns lsp-mcp.config-test
  "Tests for lsp-mcp.config (hive-di defconfig refactor).

   Verifies:
   - Generated artifacts exist (fields, schema, resolver).
   - Defaults resolve correctly when env is empty.
   - Env overrides take priority.
   - Accessors (cache-dir, workspace-root) round-trip through the cache.
   - reload! re-reads env between calls."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lsp-mcp.config :as config :refer [LspConfig-fields
                                               LspConfig-schema
                                               resolve-LspConfig]]
            [hive-dsl.result :as r]))

;; =============================================================================
;; Fixtures — isolate cache between tests
;; =============================================================================

(defn- reset-cache-fixture [f]
  (try
    (f)
    (finally
      ;; Force re-resolution against the real env so other tests start clean.
      (config/reload!))))

(use-fixtures :each reset-cache-fixture)

;; =============================================================================
;; Generated Artifacts
;; =============================================================================

(deftest field-registry-exists
  (is (map? LspConfig-fields))
  (is (= 2 (count LspConfig-fields)))
  (is (contains? LspConfig-fields :cache-dir))
  (is (contains? LspConfig-fields :workspace-root)))

(deftest schema-generated
  (is (vector? LspConfig-schema))
  (is (= :map (first LspConfig-schema))))

(deftest field-specs-typed-as-string
  (is (= :string (get-in LspConfig-fields [:cache-dir :type])))
  (is (= :string (get-in LspConfig-fields [:workspace-root :type]))))

;; =============================================================================
;; Default Resolution
;; =============================================================================

(deftest resolve-defaults-only
  (testing "Both fields resolve to ~-based defaults when env is empty"
    (let [result (resolve-LspConfig {} {:env-fn (constantly nil)})]
      (is (r/ok? result))
      (let [cfg  (:ok result)
            home (System/getProperty "user.home")]
        (is (= (str home "/.cache/hive-lsp") (:cache-dir cfg)))
        (is (= (str home "/PP")              (:workspace-root cfg)))))))

;; =============================================================================
;; Env Overrides
;; =============================================================================

(deftest resolve-with-env-overrides
  (testing "Env vars override defaults"
    (let [mock-env {"LSP_CACHE_DIR"           "/var/cache/hive-lsp"
                    "HIVE_LSP_WORKSPACE_ROOT" "/srv/workspace"}
          result   (resolve-LspConfig {} {:env-fn #(get mock-env %)})]
      (is (r/ok? result))
      (let [cfg (:ok result)]
        (is (= "/var/cache/hive-lsp" (:cache-dir cfg)))
        (is (= "/srv/workspace"      (:workspace-root cfg)))))))

(deftest resolve-with-partial-env
  (testing "Unset env vars fall back to defaults"
    (let [mock-env {"LSP_CACHE_DIR" "/custom/cache"}
          result   (resolve-LspConfig {} {:env-fn #(get mock-env %)})
          home     (System/getProperty "user.home")]
      (is (r/ok? result))
      (let [cfg (:ok result)]
        (is (= "/custom/cache" (:cache-dir cfg)))
        (is (= (str home "/PP") (:workspace-root cfg)))))))

(deftest resolve-with-overrides-map
  (testing "Explicit overrides (e.g., from addon manifest) win over env"
    (let [mock-env   {"LSP_CACHE_DIR" "/from/env"}
          overrides  {:cache-dir "/from/override"}
          result     (resolve-LspConfig overrides {:env-fn #(get mock-env %)})]
      (is (r/ok? result))
      (is (= "/from/override" (:cache-dir (:ok result)))))))

;; =============================================================================
;; Accessors + Cache Behavior
;; =============================================================================

(deftest accessors-return-strings
  (testing "cache-dir / workspace-root return non-blank strings"
    (config/reload!)
    (is (string? (config/cache-dir)))
    (is (string? (config/workspace-root)))
    (is (seq (config/cache-dir)))
    (is (seq (config/workspace-root)))))

(deftest current-returns-resolved-map
  (testing "current returns a map with both fields"
    (config/reload!)
    (let [c (config/current)]
      (is (map? c))
      (is (contains? c :cache-dir))
      (is (contains? c :workspace-root)))))

(deftest reload-re-reads-env
  (testing "reload! picks up env changes between calls"
    ;; Seed the cache.
    (config/reload!)
    (let [before (config/cache-dir)]
      ;; Simulate an env change by redefining the resolver. We can't set
      ;; real env vars inside a JVM, so we rebind resolve-LspConfig to a
      ;; constant ok result and check that reload! propagates.
      (with-redefs [config/resolve-LspConfig
                    (fn
                      ([] (r/ok {:cache-dir      "/redef/cache"
                                 :workspace-root "/redef/workspace"}))
                      ([_] (r/ok {:cache-dir      "/redef/cache"
                                  :workspace-root "/redef/workspace"}))
                      ([_ _] (r/ok {:cache-dir      "/redef/cache"
                                    :workspace-root "/redef/workspace"})))]
        (config/reload!)
        (is (= "/redef/cache"     (config/cache-dir)))
        (is (= "/redef/workspace" (config/workspace-root)))
        (is (not= before (config/cache-dir)))))))

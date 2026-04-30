(ns lsp-mcp.deps-test
  (:require [clojure.test :refer [deftest is testing]]
            [lsp-mcp.deps :as deps]))

;; -----------------------------------------------------------------------------
;; parse-deps-edn

(deftest parse-deps-edn-roundtrip
  (let [src "{:paths [\"src\"] :deps {a/b {:mvn/version \"1.0\"}}}"]
    (is (= {:paths ["src"] :deps {'a/b {:mvn/version "1.0"}}}
           (deps/parse-deps-edn src)))))

(deftest parse-deps-edn-malformed-returns-nil
  (is (nil? (deps/parse-deps-edn "{:paths"))))

;; -----------------------------------------------------------------------------
;; local-root-deps

(deftest local-root-deps-empty-when-none
  (is (= [] (deps/local-root-deps
             {:deps {'a/b {:mvn/version "1.0"}
                     'c/d {:git/sha "abc"}}}))))

(deftest local-root-deps-finds-top-level
  (is (= [{:dep 'a/b :path "../foo"}]
         (deps/local-root-deps
          {:deps {'a/b {:local/root "../foo"}
                  'c/d {:mvn/version "1.0"}}}))))

(deftest local-root-deps-finds-alias-scoped
  (testing "extra-deps under aliases are scanned too"
    (let [parsed {:deps    {'a/b {:mvn/version "1.0"}}
                  :aliases {:test {:extra-deps {'t/runner {:local/root "../t"}}}
                            :dev  {:extra-deps {'d/lib    {:local/root "/abs/d"}}}}}
          result (deps/local-root-deps parsed)
          paths  (set (map :path result))]
      (is (= 2 (count result)))
      (is (contains? paths "../t"))
      (is (contains? paths "/abs/d")))))

(deftest local-root-deps-mixes-top-and-alias
  (let [parsed {:deps    {'top/lib  {:local/root "../top"}
                          'pure/lib {:mvn/version "1"}}
                :aliases {:test {:extra-deps {'test/lib {:local/root "../t"}}}}}
        result (deps/local-root-deps parsed)]
    (is (= 2 (count result)))
    (is (= #{'top/lib 'test/lib} (set (map :dep result))))))

;; -----------------------------------------------------------------------------
;; unreachable-roots

(deftest unreachable-roots-relative-inside-workspace
  (testing "relative path resolving INSIDE workspace is reachable (dropped)"
    (is (= []
           (deps/unreachable-roots
            [{:dep 'a/b :path "../sibling"}]
            "/ws/proj"
            "/ws")))))

(deftest unreachable-roots-relative-outside-workspace
  (testing "relative path resolving OUTSIDE workspace is unreachable"
    (let [result (deps/unreachable-roots
                  [{:dep 'a/b :path "../../escape"}]
                  "/ws/proj"
                  "/ws")]
      (is (= 1 (count result)))
      (is (= 'a/b (:dep (first result))))
      (is (= "/escape" (:resolved (first result)))))))

(deftest unreachable-roots-absolute-outside
  (let [result (deps/unreachable-roots
                [{:dep 'a/b :path "/elsewhere/lib"}]
                "/ws/proj"
                "/ws")]
    (is (= 1 (count result)))
    (is (= "/elsewhere/lib" (:resolved (first result))))))

(deftest unreachable-roots-absolute-inside
  (is (= []
         (deps/unreachable-roots
          [{:dep 'a/b :path "/ws/some/dep"}]
          "/ws/proj"
          "/ws"))))

(deftest unreachable-roots-workspace-itself-is-reachable
  (testing "path that resolves to the workspace root itself is reachable"
    (is (= []
           (deps/unreachable-roots
            [{:dep 'a/b :path "/ws"}]
            "/ws/proj"
            "/ws")))))

(deftest unreachable-roots-prefix-not-substring
  (testing "/ws-other must not count as inside /ws"
    (let [result (deps/unreachable-roots
                  [{:dep 'a/b :path "/ws-other/lib"}]
                  "/ws/proj"
                  "/ws")]
      (is (= 1 (count result)))
      (is (= "/ws-other/lib" (:resolved (first result)))))))

(deftest unreachable-roots-preserves-order
  (let [entries [{:dep 'a/1 :path "/elsewhere/a"}
                 {:dep 'b/2 :path "/ws/inside"}
                 {:dep 'c/3 :path "/elsewhere/c"}]
        result  (deps/unreachable-roots entries "/ws/proj" "/ws")]
    (is (= ['a/1 'c/3] (mapv :dep result)))))

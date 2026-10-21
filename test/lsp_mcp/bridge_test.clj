(ns lsp-mcp.bridge-test
  "The live-bridge registry: lsp-mcp names no vessel, a vessel registers."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [lsp-mcp.bridge :as bridge]
            [lsp-mcp.tools :as tools]))

(use-fixtures :each
  (fn [t]
    (let [before (bridge/registered-bridges)]
      (doseq [k (keys before)] (bridge/unregister-bridge! k))
      (try (t)
           (finally
             (doseq [k (keys (bridge/registered-bridges))] (bridge/unregister-bridge! k))
             (doseq [[k b] before] (bridge/register-bridge! k b)))))))

(defn- recording-bridge [calls available?]
  (bridge/fn-bridge {:available? (fn [] @available?)
                     :call       (fn [method & args] (swap! calls conj (into [method] args)) {:method method})}))

(deftest no-bridge-registered-means-unavailable-not-a-crash
  (is (nil? (bridge/active-bridge)))
  (let [res (tools/handle-lsp {:command "hover" :project_root "/p" :file_path "/p/a.clj" :line 1 :column 2})]
    (is (:isError res))
    (is (re-find #"bridge/unavailable" (get-in res [:content 0 :text])))))

(deftest a-registered-bridge-answers-every-live-command
  (let [calls (atom [])
        b     (recording-bridge calls (atom true))]
    (bridge/register-bridge! :test b)
    (is (= b (bridge/active-bridge)))
    (doseq [[cmd method] [["bridge-status" "status"] ["workspaces" "workspaces"]
                          ["hover" "hover"] ["definition" "definition"]
                          ["live-references" "references"] ["symbols" "document-symbols"]
                          ["cursor-info" "cursor-info"] ["server-info" "server-info"]]]
      (testing cmd
        (let [res (tools/handle-lsp {:command cmd :project_root "/p" :file_path "/p/a.clj" :line 1 :column 2})]
          (is (not (:isError res)))
          (is (= method (first (last @calls)))))))))

(deftest a-bridge-that-comes-up-later-is-picked-up-without-a-restart
  (let [up (atom false)]
    (bridge/register-bridge! :late (recording-bridge (atom []) up))
    (is (nil? (bridge/active-bridge)) "down: not offered")
    (reset! up true)
    (is (some? (bridge/active-bridge)) "up: offered on the next call")))

(deftest an-availability-check-that-throws-counts-as-down
  (bridge/register-bridge! :broken (bridge/fn-bridge {:available? #(throw (ex-info "boom" {}))
                                                      :call       (constantly nil)}))
  (is (nil? (bridge/active-bridge))))

(deftest only-bridges-can-register
  (is (thrown? clojure.lang.ExceptionInfo (bridge/register-bridge! :nope {}))))

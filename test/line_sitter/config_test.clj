(ns line-sitter.config-test
  (:require
   [babashka.fs :as fs]
   [clojure.test :refer [deftest is testing]]
   [line-sitter.config :as config]
   [line-sitter.test-util :refer [with-temp-dir]]))

;; Tests for configuration loading, merging, and validation.
;; Contracts tested:
;; - default-config has expected shape
;; - find-config-file walks up directory tree correctly
;; - load-config merges user config with defaults
;; - validate-config rejects invalid configurations

(deftest default-config-test
  (testing "default-config"
    (testing "has :line-length of 80"
      (is (= 80 (:line-length config/default-config))))
    (testing "has :extensions for Clojure files"
      (is (= [".clj" ".cljs" ".cljc" ".edn"]
             (:extensions config/default-config))))
    (testing "has empty :indents map"
      (is (= {} (:indents config/default-config))))))

(deftest find-config-file-test
  (testing "find-config-file"
    (testing "when config exists in start directory"
      (testing "returns the config path"
        (with-temp-dir [root]
          (let [config-path (fs/path root ".line-sitter.edn")]
            (spit (str config-path) "{}")
            (is (= (str config-path)
                   (config/find-config-file root)))))))
    (testing "when config exists in parent directory"
      (testing "returns the parent config path"
        (with-temp-dir [root]
          (let [config-path (fs/path root ".line-sitter.edn")
                child-dir (fs/path root "child")]
            (fs/create-dir child-dir)
            (spit (str config-path) "{}")
            (is (= (str config-path)
                   (config/find-config-file child-dir)))))))
    (testing "when config exists in grandparent directory"
      (testing "returns the grandparent config path"
        (with-temp-dir [root]
          (let [config-path (fs/path root ".line-sitter.edn")
                child-dir (fs/path root "child" "grandchild")]
            (fs/create-dirs child-dir)
            (spit (str config-path) "{}")
            (is (= (str config-path)
                   (config/find-config-file child-dir)))))))
    (testing "when no config exists"
      (testing "returns nil"
        (with-temp-dir [root]
          (is (nil? (config/find-config-file root))))))))

(deftest load-config-test
  (testing "load-config"
    (testing "when config overrides :line-length"
      (testing "uses the override value"
        (with-temp-dir [root]
          (let [config-path (fs/path root ".line-sitter.edn")]
            (spit (str config-path) "{:line-length 120}")
            (is (= 120 (:line-length (config/load-config (str config-path)))))))))
    (testing "when config does not override :line-length"
      (testing "uses the default value"
        (with-temp-dir [root]
          (let [config-path (fs/path root ".line-sitter.edn")]
            (spit (str config-path) "{}")
            (is (= 80 (:line-length (config/load-config (str config-path)))))))))
    (testing "when config overrides :extensions"
      (testing "replaces the default value"
        (with-temp-dir [root]
          (let [config-path (fs/path root ".line-sitter.edn")]
            (spit (str config-path) "{:extensions [\".clj\"]}")
            (is (= [".clj"] (:extensions (config/load-config (str config-path)))))))))
    (testing "when config has :indents"
      (testing "merges with default empty map"
        (with-temp-dir [root]
          (let [config-path (fs/path root ".line-sitter.edn")]
            (spit (str config-path) "{:indents {defn 1}}")
            (is (= '{defn 1} (:indents (config/load-config (str config-path)))))))))
    (testing "when config file is not valid EDN"
      (testing "throws ex-info with :type :config-error"
        (with-temp-dir [root]
          (let [config-path (fs/path root ".line-sitter.edn")]
            (spit (str config-path) "{:a [}")
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"Failed to read config"
                 (config/load-config (str config-path))))))))
    (testing "when config file contains non-map"
      (testing "throws ex-info with :type :config-error"
        (with-temp-dir [root]
          (let [config-path (fs/path root ".line-sitter.edn")]
            (spit (str config-path) "42")
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"file must contain a map"
                 (config/load-config (str config-path))))))))))

(deftest validate-config-test
  (testing "validate-config"
    (testing "when config is valid"
      (testing "returns the config unchanged"
        (is (= config/default-config
               (config/validate-config config/default-config)))))
    (testing "when :line-length is not a positive integer"
      (testing "with zero value"
        (testing "throws ex-info"
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #":line-length must be a positive integer"
               (config/validate-config (assoc config/default-config :line-length 0))))))
      (testing "with negative value"
        (testing "throws ex-info"
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #":line-length must be a positive integer"
               (config/validate-config (assoc config/default-config :line-length -1))))))
      (testing "with string value"
        (testing "throws ex-info"
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #":line-length must be a positive integer"
               (config/validate-config (assoc config/default-config :line-length "80")))))))
    (testing "when :extensions is not a vector of strings"
      (testing "with non-vector"
        (testing "throws ex-info"
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #":extensions must be a vector of strings"
               (config/validate-config (assoc config/default-config :extensions "clj"))))))
      (testing "with vector containing non-strings"
        (testing "throws ex-info"
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #":extensions must be a vector of strings"
               (config/validate-config (assoc config/default-config :extensions [:clj])))))))
    (testing "when :indents is not a map"
      (testing "throws ex-info"
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #":indents must be a map"
             (config/validate-config (assoc config/default-config :indents []))))))))

(deftest deep-merge-test
  (testing "deep-merge"
    (testing "merges flat maps"
      (is (= {:a 1 :b 2}
             (config/deep-merge {:a 1} {:b 2}))))
    (testing "later values override earlier"
      (is (= {:a 2}
             (config/deep-merge {:a 1} {:a 2}))))
    (testing "deeply merges nested maps"
      (is (= {:a {:b 1 :c 2}}
             (config/deep-merge {:a {:b 1}} {:a {:c 2}}))))
    (testing "replaces non-map values"
      (is (= {:a [2]}
             (config/deep-merge {:a [1]} {:a [2]}))))))

(ns line-sitter.treesitter.language-test
  "Tests for native library discovery and language loading.

  Tests verify:
  - Successful loading from classpath resources
  - Environment variable discovery path
  - Informative error when library not found"
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [line-sitter.treesitter.language :as lang])
  (:import [clojure.lang ExceptionInfo]
           [io.github.treesitter.jtreesitter Language]))

(deftest load-clojure-language-test
  ;; Verify loading the Clojure grammar from bundled native library.
  ;; The pre-built library in resources/native/<os>-<arch>/ should be found.
  (testing "load-clojure-language"
    (testing "returns a Language instance from classpath resource"
      (let [language (lang/load-clojure-language)]
        (is (instance? Language language))))

    (testing "returns the same language on repeated calls"
      ;; Language loading uses global arena, so multiple loads should work
      (let [lang1 (lang/load-clojure-language)
            lang2 (lang/load-clojure-language)]
        (is (instance? Language lang1))
        (is (instance? Language lang2))))))

(deftest env-var-discovery-test
  ;; Verify that LINE_SITTER_NATIVE_LIB environment variable is checked first.
  ;; Uses with-redefs to simulate env var being set to an existing file.
  (testing "find-library-path"
    (testing "uses env var path when set and file exists"
      (let [temp-file (fs/create-temp-file {:suffix ".dylib"})]
        (try
          (with-redefs [lang/get-env-lib-path (constantly (str temp-file))]
            (let [[path source] (#'lang/find-library-path)]
              (is (= :env-var source)
                  "reports :env-var as the source")
              (is (= (str temp-file) (str path))
                  "returns the env var path")))
          (finally
            (fs/delete temp-file)))))

    (testing "falls back to classpath when env var path does not exist"
      (with-redefs [lang/get-env-lib-path (constantly "/nonexistent/lib.dylib")]
        (let [[_path source] (#'lang/find-library-path)]
          (is (= :classpath source)
              "falls back to classpath when env var path missing"))))))

(deftest library-not-found-test
  ;; Verify informative error when native library cannot be found.
  ;; Uses with-redefs to simulate missing library scenario.
  (testing "find-library-path"
    (testing "throws ex-info with useful data when library not found"
      (with-redefs [lang/get-env-lib-path (constantly nil)
                    lang/extract-resource-to-temp (constantly nil)
                    lang/find-in-library-path (constantly nil)]
        (let [ex (try
                   (#'lang/find-library-path)
                   nil
                   (catch ExceptionInfo e e))]
          (is (instance? ExceptionInfo ex)
              "throws ExceptionInfo when library not found")
          (when ex
            (let [data (ex-data ex)]
              (is (contains? data :library)
                  "ex-data includes :library")
              (is (contains? data :os)
                  "ex-data includes :os")
              (is (contains? data :arch)
                  "ex-data includes :arch")
              (is (contains? data :resource-path)
                  "ex-data includes :resource-path"))))))))

(ns line-sitter.main-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [line-sitter.main :as main]
   [line-sitter.test-util :refer [with-captured-output with-temp-dir]]))

;; Integration tests for main entry point. Verifies the full flow:
;; parse args → load config → resolve files → process → exit code.
;; Tests cover help output, check/fix/stdout modes, config loading,
;; and error handling.

(deftest help-test
  (testing "run with --help"
    (testing "prints usage text to stdout"
      (let [[out _err exit-code] (with-captured-output
                                   (main/run ["--help"]))]
        (is (str/includes? out "Usage: line-sitter"))
        (is (str/includes? out "--check"))
        (is (str/includes? out "--fix"))
        (is (str/includes? out "--stdout"))
        (is (str/includes? out "--line-length"))
        (is (= 0 exit-code))))

    (testing "with -h alias prints usage"
      (let [[out _err exit-code] (with-captured-output
                                   (main/run ["-h"]))]
        (is (str/includes? out "Usage: line-sitter"))
        (is (= 0 exit-code))))))

(deftest check-mode-test
  (testing "run in check mode"
    (testing "exits 0 with valid files"
      (with-temp-dir [root]
        (let [file (fs/path root "test.clj")]
          (spit (str file) "(ns test)")
          (let [[_out _err exit-code] (with-captured-output
                                        (main/run ["--check" (str file)]))]
            (is (= 0 exit-code))))))

    (testing "exits 0 with directory"
      (with-temp-dir [root]
        (let [file (fs/path root "test.clj")]
          (spit (str file) "(ns test)")
          (let [[_out _err exit-code] (with-captured-output
                                        (main/run ["--check" (str root)]))]
            (is (= 0 exit-code))))))

    (testing "is the default mode"
      (with-temp-dir [root]
        (let [file (fs/path root "test.clj")]
          (spit (str file) "(ns test)")
          (let [[_out _err exit-code] (with-captured-output
                                        (main/run [(str file)]))]
            (is (= 0 exit-code))))))))

(deftest fix-mode-test
  (testing "run in fix mode"
    (testing "exits 0 with valid files"
      (with-temp-dir [root]
        (let [file (fs/path root "test.clj")]
          (spit (str file) "(ns test)")
          (let [[_out _err exit-code] (with-captured-output
                                        (main/run ["--fix" (str file)]))]
            (is (= 0 exit-code))))))))

(deftest stdout-mode-test
  (testing "run in stdout mode"
    (testing "with single file"
      (testing "echoes file content without header"
        (with-temp-dir [root]
          (let [file (fs/path root "test.clj")
                content "(ns test)\n"]
            (spit (str file) content)
            (let [[out _err exit-code] (with-captured-output
                                         (main/run ["--stdout" (str file)]))]
              (is (= content out))
              (is (= 0 exit-code)))))))

    (testing "with multiple files"
      (testing "prefixes each with ;;; path header"
        (with-temp-dir [root]
          (let [file1 (fs/path root "a.clj")
                file2 (fs/path root "b.clj")]
            (spit (str file1) "(ns a)")
            (spit (str file2) "(ns b)")
            (let [[out _err exit-code] (with-captured-output
                                         (main/run ["--stdout" (str root)]))]
              (is (str/includes? out ";;; "))
              (is (str/includes? out "(ns a)"))
              (is (str/includes? out "(ns b)"))
              (is (= 0 exit-code)))))))))

(deftest config-loading-test
  (testing "run with config file"
    (testing "loads config from file directory"
      (with-temp-dir [root]
        (let [config-path (fs/path root ".line-sitter.edn")
              file (fs/path root "test.clj")]
          (spit (str config-path) "{:line-length 120}")
          (spit (str file) "(ns test)")
          ;; Should not error - config is valid
          (let [[_out _err exit-code] (with-captured-output
                                        (main/run [(str file)]))]
            (is (= 0 exit-code))))))

    (testing "CLI --line-length overrides config"
      (with-temp-dir [root]
        (let [config-path (fs/path root ".line-sitter.edn")
              file (fs/path root "test.clj")]
          (spit (str config-path) "{:line-length 80}")
          (spit (str file) "(ns test)")
          ;; Should work with CLI override
          (let [[_out _err exit-code] (with-captured-output
                                        (main/run ["--line-length" "100"
                                                   (str file)]))]
            (is (= 0 exit-code))))))))

(deftest error-handling-test
  (testing "run with errors"
    (testing "given non-existent file"
      (testing "exits 2 with error message"
        (let [[_out err exit-code] (with-captured-output
                                     (main/run ["nonexistent.clj"]))]
          (is (= 2 exit-code))
          (is (str/includes? err "line-sitter: file-error:")))))

    (testing "given invalid config"
      (testing "exits 2 with error message"
        (with-temp-dir [root]
          (let [config-path (fs/path root ".line-sitter.edn")
                file (fs/path root "test.clj")]
            (spit (str config-path) "{:line-length -1}")
            (spit (str file) "(ns test)")
            (let [[_out err exit-code] (with-captured-output
                                         (main/run [(str file)]))]
              (is (= 2 exit-code))
              (is (str/includes? err "line-sitter: config-error:")))))))))

(deftest format-error-test
  (testing "format-error"
    (testing "formats error with type and message"
      (is (= "line-sitter: config-error: bad value"
             (main/format-error "config-error" "bad value"))))))

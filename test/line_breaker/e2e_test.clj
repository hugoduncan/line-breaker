(ns line-breaker.e2e-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [line-breaker.main :as main]
   [line-breaker.test-util :refer [with-captured-output]]))

;; End-to-end tests using static fixtures in test-resources/.
;; These tests verify the complete flow from CLI args to exit code
;; using pre-configured directories that test config loading, file
;; discovery, and extensions filtering.

;;; Config search tests

(deftest config-search-from-nested-dir-test
  ;; Tests that config search walks up directory tree to find
  ;; .line-breaker.edn in parent directories.
  (testing "run with file in nested directory"
    (testing "finds config in parent directory"
      ;; The config in cli-test/ sets extensions to [".clj" ".cljs"]
      ;; so .edn files should be excluded
      (let [[_out _err exit-code]
            (with-captured-output
              (main/run ["test-resources/cli-test/nested/deep/bottom.clj"]))]
        (is (= 0 exit-code))))))

;;; Extensions filtering tests

(deftest extensions-filtering-test
  ;; Tests that extensions from config filter discovered files.
  ;; cli-test/.line-breaker.edn has :extensions [".clj" ".cljs"]
  (testing "run on directory with config"
    (testing "excludes files not matching configured extensions"
      (let [[out _err exit-code]
            (with-captured-output
              (main/run ["--stdout" "test-resources/cli-test"]))]
        ;; Should find .clj and .cljs but not .edn
        (is (= 0 exit-code))
        (is (str/includes? out "(ns sample)"))
        (is (str/includes? out "(ns nested.inner)"))
        (is (str/includes? out "(ns nested.deep.bottom)"))
        (is (not (str/includes? out "should \"be excluded")))))))

;;; Error format tests

(deftest error-format-test
  ;; Tests that errors are printed to stderr in the correct format:
  ;; "line-breaker: <category>: <details>"
  (testing "run with invalid config"
    (testing "prints error to stderr in correct format"
      (let [[_out err exit-code]
            (with-captured-output
              (main/run ["test-resources/cli-test-invalid"]))]
        (is (= 2 exit-code))
        (is (str/starts-with? err "line-breaker:"))
        (is (str/includes? err "config-error:"))))))

;;; Full workflow tests

(deftest check-mode-e2e-test
  ;; Verifies --check mode exits 0 (no violations in no-op mode).
  (testing "run --check on fixtures"
    (testing "exits 0"
      (let [[_out _err exit-code]
            (with-captured-output
              (main/run ["--check" "test-resources/cli-test"]))]
        (is (= 0 exit-code))))))

(deftest fix-mode-e2e-test
  ;; Verifies --fix mode exits 0 silently (no files modified in no-op).
  (testing "run --fix on fixtures"
    (testing "exits 0 silently"
      (let [[out _err exit-code]
            (with-captured-output
              (main/run ["--fix" "test-resources/cli-test"]))]
        (is (= 0 exit-code))
        (is (= "" out))))))

(deftest stdout-mode-e2e-test
  ;; Verifies --stdout echoes file contents with headers for multiple files.
  (testing "run --stdout on fixtures"
    (testing "outputs file contents with headers"
      (let [[out _err exit-code]
            (with-captured-output
              (main/run ["--stdout" "test-resources/cli-test"]))]
        (is (= 0 exit-code))
        (is (str/includes? out ";;;"))
        (is (str/includes? out "(ns sample)"))))))

(deftest line-length-override-test
  ;; Verifies --line-length CLI option overrides config.
  (testing "run with --line-length override"
    (testing "accepts the override"
      ;; Config has :line-length 100, override to 120
      (let [[_out _err exit-code]
            (with-captured-output
              (main/run ["--line-length" "120"
                         "test-resources/cli-test/sample.clj"]))]
        (is (= 0 exit-code))))))

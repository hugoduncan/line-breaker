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
    (testing "given files with no violations"
      (testing "exits 0"
        (with-temp-dir [root]
          (let [file (fs/path root "test.clj")]
            (spit (str file) "(ns test)")
            (let [[_out _err exit-code] (with-captured-output
                                          (main/run ["--check" (str file)]))]
              (is (= 0 exit-code)))))))

    (testing "given directory with no violations"
      (testing "exits 0"
        (with-temp-dir [root]
          (let [file (fs/path root "test.clj")]
            (spit (str file) "(ns test)")
            (let [[_out _err exit-code] (with-captured-output
                                          (main/run ["--check" (str root)]))]
              (is (= 0 exit-code)))))))

    (testing "without explicit --check flag"
      (testing "defaults to check mode"
        (with-temp-dir [root]
          (let [file (fs/path root "test.clj")]
            (spit (str file) "(ns test)")
            (let [[_out _err exit-code] (with-captured-output
                                          (main/run [(str file)]))]
              (is (= 0 exit-code)))))))

    (testing "given file with violations"
      (testing "exits 1 and reports to stderr"
        (with-temp-dir [root]
          (let [file (fs/path root "long.clj")
                long-line (apply str (repeat 100 "x"))]
            (spit (str file) (str "(ns long)\n" long-line "\n"))
            (let [[_out err exit-code] (with-captured-output
                                         (main/run ["--check" (str file)]))]
              (is (= 1 exit-code))
              (is (str/includes? err "line exceeds 80 characters"))
              (is (str/includes? err ":2:")))))))

    (testing "given mixed files"
      (testing "exits 1 and reports only violations"
        (with-temp-dir [root]
          (let [good-file (fs/path root "good.clj")
                bad-file (fs/path root "bad.clj")
                long-line (apply str (repeat 100 "y"))]
            (spit (str good-file) "(ns good)")
            (spit (str bad-file) (str "(ns bad)\n" long-line "\n"))
            (let [[_out err exit-code] (with-captured-output
                                         (main/run ["--check" (str root)]))]
              (is (= 1 exit-code))
              (is (str/includes? err "bad.clj"))
              (is (not (str/includes? err "good.clj"))))))))

    (testing "given empty file list"
      (testing "exits 0"
        (with-temp-dir [root]
          ;; Create an empty directory with no .clj files
          (let [[_out _err exit-code] (with-captured-output
                                        (main/run ["--check" (str root)]))]
            (is (= 0 exit-code))))))

    (testing "with --line-length override"
      (testing "uses CLI value over default"
        (with-temp-dir [root]
          (let [file (fs/path root "test.clj")
                ;; 50 chars - would violate default 80, but not 60
                line (apply str (repeat 50 "z"))]
            (spit (str file) line)
            (let [[_out _err exit-code] (with-captured-output
                                          (main/run ["--line-length" "60"
                                                     "--check" (str file)]))]
              (is (= 0 exit-code)))))))

    (testing "summary output"
      (testing "given multiple files with no violations"
        (testing "prints summary to stderr"
          (with-temp-dir [root]
            (let [file1 (fs/path root "a.clj")
                  file2 (fs/path root "b.clj")]
              (spit (str file1) "(ns a)")
              (spit (str file2) "(ns b)")
              (let [[_out err exit-code] (with-captured-output
                                           (main/run ["--check" (str root)]))]
                (is (= 0 exit-code))
                (is (str/includes? err "Checked 2 files"))
                (is (str/includes? err "all lines within limit")))))))

      (testing "given multiple files with violations"
        (testing "prints summary with count"
          (with-temp-dir [root]
            (let [file1 (fs/path root "a.clj")
                  file2 (fs/path root "b.clj")
                  long-line (apply str (repeat 100 "x"))]
              (spit (str file1) "(ns a)")
              (spit (str file2) (str "(ns b)\n" long-line))
              (let [[_out err exit-code] (with-captured-output
                                           (main/run ["--check" (str root)]))]
                (is (= 1 exit-code))
                (is (str/includes? err "Checked 2 files"))
                (is (str/includes? err "1 violation found")))))))

      (testing "given single file"
        (testing "does not print summary"
          (with-temp-dir [root]
            (let [file (fs/path root "test.clj")]
              (spit (str file) "(ns test)")
              (let [[_out err exit-code] (with-captured-output
                                           (main/run ["--check" (str file)]))]
                (is (= 0 exit-code))
                (is (not (str/includes? err "Checked")))))))))

    (testing "with --quiet flag"
      (testing "suppresses summary output"
        (with-temp-dir [root]
          (let [file1 (fs/path root "a.clj")
                file2 (fs/path root "b.clj")]
            (spit (str file1) "(ns a)")
            (spit (str file2) "(ns b)")
            (let [[_out err exit-code] (with-captured-output
                                         (main/run ["--check" "--quiet"
                                                    (str root)]))]
              (is (= 0 exit-code))
              (is (not (str/includes? err "Checked")))))))

      (testing "with -q alias"
        (testing "suppresses summary output"
          (with-temp-dir [root]
            (let [file1 (fs/path root "a.clj")
                  file2 (fs/path root "b.clj")]
              (spit (str file1) "(ns a)")
              (spit (str file2) "(ns b)")
              (let [[_out err exit-code] (with-captured-output
                                           (main/run ["--check" "-q" (str root)]))]
                (is (= 0 exit-code))
                (is (not (str/includes? err "Checked")))))))))))

(deftest fix-mode-test
  ;; Tests that --fix mode writes fixed content back to files.
  ;; Verifies file modification, output messages, and quiet mode.
  (testing "run in fix mode"
    (testing "given file with no violations"
      (testing "exits 0 and does not modify file"
        (with-temp-dir [root]
          (let [file (fs/path root "test.clj")
                content "(ns test)"]
            (spit (str file) content)
            (let [[_out _err exit-code] (with-captured-output
                                          (main/run ["--fix" (str file)]))]
              (is (= 0 exit-code))
              (is (= content (slurp (str file)))))))))

    (testing "given file with long line"
      (testing "rewrites file with breaking applied"
        (with-temp-dir [root]
          (let [file (fs/path root "long.clj")
                ;; Construct 90-char list (exceeds 80)
                long-form (str "(foo " (str/join " " (repeat 20 "arg")) ")")]
            (spit (str file) long-form)
            (let [[_out err exit-code] (with-captured-output
                                         (main/run ["--fix" (str file)]))]
              (is (= 0 exit-code))
              (is (str/includes? err "Fixed:"))
              (let [result (slurp (str file))]
                ;; Verify breaking happened
                (is (str/includes? result "\n"))
                ;; Verify elements present
                (is (str/includes? result "foo"))
                (is (str/includes? result "arg"))))))))

    (testing "with --quiet flag"
      (testing "suppresses Fixed: output"
        (with-temp-dir [root]
          (let [file (fs/path root "long.clj")
                long-form (str "(foo " (str/join " " (repeat 20 "arg")) ")")]
            (spit (str file) long-form)
            (let [[_out err exit-code] (with-captured-output
                                         (main/run ["--fix" "--quiet"
                                                    (str file)]))]
              (is (= 0 exit-code))
              (is (not (str/includes? err "Fixed:"))))))))))

(deftest stdout-mode-test
  ;; Tests that --stdout mode outputs fixed content without modifying files.
  ;; Verifies stdout output and multi-file headers.
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

    (testing "with long line"
      (testing "outputs fixed content without modifying file"
        (with-temp-dir [root]
          (let [file (fs/path root "long.clj")
                long-form (str "(foo " (str/join " " (repeat 20 "arg")) ")")]
            (spit (str file) long-form)
            (let [[out _err exit-code] (with-captured-output
                                         (main/run ["--stdout" (str file)]))]
              (is (= 0 exit-code))
              ;; Output should have breaking
              (is (str/includes? out "\n"))
              ;; File should be unchanged
              (is (= long-form (slurp (str file)))))))))

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

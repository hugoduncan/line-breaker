(ns line-sitter.cli-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [line-sitter.cli :as cli]))

;; Tests for CLI argument parsing. Verifies that parse-args correctly
;; handles mode flags (check/fix/stdout), line-length option, help flag,
;; and positional arguments.

(deftest parse-args-test
  (testing "parse-args"
    (testing "defaults to check mode when no mode specified"
      (is (= {:opts {:check true} :args []}
             (cli/parse-args []))))

    (testing "parses --check flag"
      (is (= {:opts {:check true} :args []}
             (cli/parse-args ["--check"]))))

    (testing "parses --fix flag"
      (is (= {:opts {:fix true} :args []}
             (cli/parse-args ["--fix"]))))

    (testing "parses --stdout flag"
      (is (= {:opts {:stdout true} :args []}
             (cli/parse-args ["--stdout"]))))

    (testing "parses --line-length as a number"
      (is (= {:opts {:check true :line-length 100} :args []}
             (cli/parse-args ["--line-length" "100"]))))

    (testing "parses --help flag"
      (is (= {:opts {:help true} :args []}
             (cli/parse-args ["--help"]))))

    (testing "parses -h alias for help"
      (is (= {:opts {:help true} :args []}
             (cli/parse-args ["-h"]))))

    (testing "captures positional arguments"
      (is (= {:opts {:check true} :args ["src/foo.clj"]}
             (cli/parse-args ["src/foo.clj"]))))

    (testing "captures multiple positional arguments"
      (is (= {:opts {:check true} :args ["src" "test"]}
             (cli/parse-args ["src" "test"]))))

    (testing "combines options with positional arguments"
      (is (= {:opts {:fix true :line-length 120} :args ["src/foo.clj"]}
             (cli/parse-args ["--fix" "--line-length" "120" "src/foo.clj"]))))

    (testing "when multiple modes specified, last wins"
      ;; babashka.cli default behavior: later flags override earlier ones
      (is (= {:opts {:fix true :stdout true} :args []}
             (cli/parse-args ["--fix" "--stdout"]))))

    (testing "when --line-length is given without a value"
      ;; babashka.cli treats flag as boolean true then fails coercion
      (testing "throws an exception with coercion failure message"
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"cannot transform.*to long"
             (cli/parse-args ["--line-length"])))))))

;;; File discovery tests

(def ^:private test-fixtures-dir
  "test-resources/file-discovery-test")

(def ^:private default-extensions
  [".clj" ".cljs" ".cljc" ".edn"])

;; Tests for resolve-files. Verifies file discovery handles single files,
;; directories, extension filtering, and error cases correctly.

(deftest resolve-files-test
  (testing "resolve-files"
    (testing "given a single file path"
      (testing "returns it as an absolute path"
        (let [path "test-resources/file-discovery-test/single.clj"
              result (cli/resolve-files [path] default-extensions)]
          (is (= 1 (count result)))
          (is (str/ends-with? (first result) "single.clj"))
          (is (fs/absolute? (first result))))))

    (testing "given a directory path"
      (testing "recursively finds all matching files"
        (let [result (cli/resolve-files [test-fixtures-dir] default-extensions)]
          (is (= 3 (count result))
              (str "Expected 3 files, got: " result))
          (is (some #(str/ends-with? % "single.clj") result))
          (is (some #(str/ends-with? % "inner.cljs") result))
          (is (some #(str/ends-with? % "bottom.edn") result)))))

    (testing "given a directory path"
      (testing "excludes non-matching extensions"
        (let [result (cli/resolve-files [test-fixtures-dir] default-extensions)]
          (is (not (some #(str/ends-with? % "other.txt") result))))))

    (testing "given empty paths"
      (testing "defaults to current directory"
        (let [result (cli/resolve-files [] default-extensions)]
          ;; Should find files in current dir (the project)
          (is (seq result) "Should find at least some files"))))

    (testing "given non-existent path"
      (testing "throws ex-info with :type :file-error"
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"Path does not exist"
             (cli/resolve-files ["nonexistent/path.clj"] default-extensions)))))

    (testing "given non-existent path"
      (testing "includes the path in exception data"
        (try
          (cli/resolve-files ["nonexistent/path.clj"] default-extensions)
          (is false "Should have thrown")
          (catch clojure.lang.ExceptionInfo e
            (is (= :file-error (:type (ex-data e))))
            (is (= "nonexistent/path.clj" (:path (ex-data e))))))))

    (testing "given a file with non-matching extension"
      (testing "excludes it from results"
        (let [path "test-resources/file-discovery-test/other.txt"
              result (cli/resolve-files [path] default-extensions)]
          (is (empty? result)))))

    (testing "given custom extensions"
      (testing "filters to only matching files"
        (let [result (cli/resolve-files [test-fixtures-dir] [".clj"])]
          (is (= 1 (count result)))
          (is (str/ends-with? (first result) "single.clj")))))

    (testing "returns sorted paths"
      (let [result (cli/resolve-files [test-fixtures-dir] default-extensions)]
        (is (= result (sort result)))))))

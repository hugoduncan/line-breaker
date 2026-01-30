(ns line-sitter.cli-test
  (:require
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
             (cli/parse-args ["--fix" "--stdout"]))))))

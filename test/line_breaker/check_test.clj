(ns line-breaker.check-test
  ;; Tests for check-line-lengths function.
  ;; Contract: returns [{:line n :length len}] for violating lines.
  (:require
   [clojure.test :refer [deftest is testing]]
   [babashka.fs :as fs]
   [line-breaker.check :as check]
   [line-breaker.test-util :refer [with-temp-dir]]))

(deftest check-line-lengths-test
  (testing "check-line-lengths"
    (testing "returns empty vector for file with no violations"
      (with-temp-dir [dir]
        (let [file (fs/file dir "short.clj")]
          (spit file "short line\nalso short\n")
          (is (= []
                 (check/check-line-lengths (str file) 80))))))

    (testing "returns single violation for file with one long line"
      (with-temp-dir [dir]
        (let [file (fs/file dir "one-long.clj")
              long-line (apply str (repeat 15 "x"))]
          (spit file (str "short\n" long-line "\nshort\n"))
          (is (= [{:line 2 :length 15}]
                 (check/check-line-lengths (str file) 10))))))

    (testing "returns multiple violations in order"
      (with-temp-dir [dir]
        (let [file (fs/file dir "multi.clj")
              line1 (apply str (repeat 12 "a"))
              line3 (apply str (repeat 15 "b"))]
          (spit file (str line1 "\nok\n" line3 "\n"))
          (is (= [{:line 1 :length 12}
                  {:line 3 :length 15}]
                 (check/check-line-lengths (str file) 10))))))

    (testing "returns empty vector for empty file"
      (with-temp-dir [dir]
        (let [file (fs/file dir "empty.clj")]
          (spit file "")
          (is (= []
                 (check/check-line-lengths (str file) 80))))))

    (testing "returns empty vector for file with only short lines"
      (with-temp-dir [dir]
        (let [file (fs/file dir "all-short.clj")]
          (spit file "a\nbb\nccc\n")
          (is (= []
                 (check/check-line-lengths (str file) 10))))))

    (testing "returns no violation for line exactly at max-length"
      (with-temp-dir [dir]
        (let [file (fs/file dir "exact.clj")
              ;; Line of exactly 10 characters (not 11)
              exact-line "1234567890"]
          (spit file (str exact-line "\n"))
          (is (= []
                 (check/check-line-lengths (str file) 10))))))))

(deftest format-violation-test
  ;; Tests formatting of violation data into user-facing error message.
  ;; Contract: format-violation takes {:file :line :length} and max-length.
  ;; Returns "path:line: line exceeds N characters (actual: M)".
  (testing "format-violation"
    (testing "formats standard violation correctly"
      (is (= "src/foo.clj:42: line exceeds 80 characters (actual: 95)"
             (check/format-violation
              {:file "src/foo.clj" :line 42 :length 95} 80))))

    (testing "formats very long line correctly"
      (is (= "test.clj:1: line exceeds 80 characters (actual: 500)"
             (check/format-violation
              {:file "test.clj" :line 1 :length 500} 80))))

    (testing "formats deep path correctly"
      (is (= "a/b/c.clj:100: line exceeds 120 characters (actual: 150)"
             (check/format-violation
              {:file "a/b/c.clj" :line 100 :length 150} 120))))

    (testing "handles different max-length values"
      (is (= "x.clj:1: line exceeds 40 characters (actual: 50)"
             (check/format-violation {:file "x.clj" :line 1 :length 50} 40))))))

(deftest report-violations-test
  ;; Tests violations are written to stderr in the correct format.
  ;; Contract: writes each violation to stderr and returns count.
  (testing "report-violations"
    (testing "writes violations to stderr"
      (let [violations [{:file "a.clj" :line 1 :length 85}
                        {:file "b.clj" :line 2 :length 90}]
            err-output (with-out-str
                         (binding [*err* *out*]
                           (check/report-violations violations 80)))]
        (is (= (str "a.clj:1: line exceeds 80 characters (actual: 85)\n"
                    "b.clj:2: line exceeds 80 characters (actual: 90)\n")
               err-output))))

    (testing "returns count of violations"
      (let [violations [{:file "a.clj" :line 1 :length 85}
                        {:file "b.clj" :line 2 :length 90}]]
        (binding [*err* (java.io.StringWriter.)]
          (is (= 2 (check/report-violations violations 80))))))

    (testing "returns zero for empty violations"
      (binding [*err* (java.io.StringWriter.)]
        (is (= 0 (check/report-violations [] 80)))))))

(deftest filter-violations-test
  ;; Tests filtering of violations based on ignored line ranges.
  ;; Contract: filter-violations removes violations whose line falls within
  ;; any [start end] range in the ignored-ranges set.
  (testing "filter-violations"
    (testing "returns all violations when no ranges"
      (let [violations [{:line 1 :length 85} {:line 5 :length 90}]]
        (is (= violations
               (check/filter-violations violations #{})))))

    (testing "removes violations within single range"
      (let [violations [{:line 1 :length 85}
                        {:line 3 :length 90}
                        {:line 5 :length 95}]]
        (is (= [{:line 1 :length 85} {:line 5 :length 95}]
               (check/filter-violations violations #{[2 4]})))))

    (testing "removes violations within multiple ranges"
      (let [violations [{:line 1 :length 85}
                        {:line 3 :length 90}
                        {:line 7 :length 95}
                        {:line 10 :length 100}]]
        (is (= [{:line 1 :length 85} {:line 10 :length 100}]
               (check/filter-violations violations #{[2 4] [6 8]})))))

    (testing "includes violation on range boundary (inclusive)"
      (let [violations [{:line 2 :length 85}
                        {:line 4 :length 90}]]
        (is (= []
               (check/filter-violations violations #{[2 4]})))))

    (testing "returns empty when all filtered"
      (let [violations [{:line 3 :length 85}]]
        (is (= []
               (check/filter-violations violations #{[1 10]})))))))

(deftest format-summary-test
  ;; Tests formatting of check summary message.
  ;; Contract: format-summary returns nil for single file, appropriate
  ;; message for multiple files based on violation count.
  (testing "format-summary"
    (testing "returns nil for single file"
      (is (nil? (check/format-summary 1 0)))
      (is (nil? (check/format-summary 1 5))))

    (testing "returns success message for multiple files with no violations"
      (is (= "Checked 2 files, all lines within limit"
             (check/format-summary 2 0)))
      (is (= "Checked 10 files, all lines within limit"
             (check/format-summary 10 0))))

    (testing "returns violation message for multiple files with violations"
      (is (= "Checked 3 files, 5 violations found"
             (check/format-summary 3 5))))

    (testing "uses singular 'violation' for count of 1"
      (is (= "Checked 100 files, 1 violation found"
             (check/format-summary 100 1))))))

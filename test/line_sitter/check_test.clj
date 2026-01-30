(ns line-sitter.check-test
  ;; Tests for check-line-lengths function.
  ;; Verifies contract: returns vector of {:line n :length len} for violating lines.
  (:require
   [clojure.test :refer [deftest is testing]]
   [babashka.fs :as fs]
   [line-sitter.check :as check]
   [line-sitter.test-util :refer [with-temp-dir]]))

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
                 (check/check-line-lengths (str file) 10))))))))

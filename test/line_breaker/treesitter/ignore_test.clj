(ns line-breaker.treesitter.ignore-test
  ;; Tests for #_:line-breaker/ignore directive support.
  ;; Verifies find-ignored-ranges correctly identifies ignore markers and their
  ;; associated forms, and check-file-with-ignore filters violations properly.
  (:require
   [clojure.test :refer [deftest is testing]]
   [babashka.fs :as fs]
   [line-breaker.check :as check]
   [line-breaker.test-util :refer [with-temp-dir]]
   [line-breaker.treesitter.parser :as parser]))

(deftest find-ignored-ranges-test
  ;; Tests that find-ignored-ranges correctly identifies line ranges
  ;; covered by #_:line-breaker/ignore markers.
  (testing "find-ignored-ranges"
    (testing "returns empty set when no ignore markers"
      (let [tree (parser/parse-source "(defn foo [x] x)")]
        (is (= #{}
               (check/find-ignored-ranges tree)))))

    (testing "finds single ignore marker"
      (let [tree (parser/parse-source
                  "#_:line-breaker/ignore (long-form)")]
        (is (= #{[1 1]}
               (check/find-ignored-ranges tree)))))

    (testing "finds multiple ignore markers"
      (let [tree (parser/parse-source
                  (str "#_:line-breaker/ignore (form1)\n"
                       "(normal)\n"
                       "#_:line-breaker/ignore (form2)"))]
        (is (= #{[1 1] [3 3]}
               (check/find-ignored-ranges tree)))))

    (testing "handles multiline ignored form"
      (let [tree (parser/parse-source
                  (str "#_:line-breaker/ignore\n"
                       "(defn foo\n"
                       "  [x]\n"
                       "  x)"))]
        (is (= #{[2 4]}
               (check/find-ignored-ranges tree)))))

    (testing "ignores marker at end of file with no sibling"
      (let [tree (parser/parse-source
                  "(form)\n#_:line-breaker/ignore")]
        (is (= #{}
               (check/find-ignored-ranges tree)))))

    (testing "handles nested ignore markers"
      ;; The second ignore marker is the sibling of the first
      (let [tree (parser/parse-source
                  "#_:line-breaker/ignore #_:line-breaker/ignore (form)")]
        ;; First ignore targets second ignore (line 1)
        ;; Second ignore targets (form) (line 1)
        (is (= #{[1 1]}
               (check/find-ignored-ranges tree)))))

    (testing "ignores other discard expressions"
      (let [tree (parser/parse-source
                  "#_:something-else (form)\n#_(other) (form2)")]
        (is (= #{}
               (check/find-ignored-ranges tree)))))))

(deftest check-file-with-ignore-test
  ;; Tests end-to-end behavior of check-file-with-ignore.
  ;; Verifies that violations in ignored forms are not reported.
  ;; Note: max-length 30 avoids ignore marker (21 chars) being a violation.
  (testing "check-file-with-ignore"
    (testing "returns violations for non-ignored lines"
      (with-temp-dir [dir]
        (let [file (fs/file dir "test.clj")
              long-line (apply str (repeat 35 "x"))]
          (spit file (str "(short)\n" long-line "\n"))
          (is (= [{:line 2 :length 35}]
                 (check/check-file-with-ignore (str file) 30))))))

    (testing "filters violations in ignored forms"
      (with-temp-dir [dir]
        (let [file (fs/file dir "test.clj")
              long-line (apply str (repeat 35 "x"))]
          (spit file (str "#_:line-breaker/ignore\n(" long-line ")\n"))
          (is (= []
                 (check/check-file-with-ignore (str file) 30))))))

    (testing "filters multiline ignored form"
      (with-temp-dir [dir]
        (let [file (fs/file dir "test.clj")
              line1 (apply str (repeat 35 "a"))
              line2 (apply str (repeat 35 "b"))]
          (spit file (str "#_:line-breaker/ignore\n"
                          "(" line1 "\n"
                          " " line2 ")\n"))
          (is (= []
                 (check/check-file-with-ignore (str file) 30))))))

    (testing "reports violations outside ignored range"
      (with-temp-dir [dir]
        (let [file (fs/file dir "test.clj")
              long-line (apply str (repeat 35 "x"))]
          (spit file (str long-line "\n"
                          "#_:line-breaker/ignore\n"
                          "(" long-line ")\n"
                          long-line "\n"))
          (is (= [{:line 1 :length 35} {:line 4 :length 35}]
                 (check/check-file-with-ignore (str file) 30))))))

    (testing "returns empty for file with no violations"
      (with-temp-dir [dir]
        (let [file (fs/file dir "test.clj")]
          (spit file "(short)\n")
          (is (= []
                 (check/check-file-with-ignore (str file) 80))))))))

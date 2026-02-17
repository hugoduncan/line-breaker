(ns line-breaker.rules-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [line-breaker.reformat]
   [line-breaker.rules]))

;; Verifies that default-indent-rules (rules.clj) and
;; default-force-break-rules (reformat.clj) cover the same symbols.
(deftest indent-and-force-break-rules-consistency-test
  (let [indent-syms (set (keys @#'line-breaker.rules/default-indent-rules))
        break-syms (set
                    (keys
                     @#'line-breaker.reformat/default-force-break-rules))
        missing-break (sort (remove break-syms indent-syms))
        missing-indent (sort (remove indent-syms break-syms))]
    (testing "default-indent-rules and default-force-break-rules"
      (testing "has a force-break rule for every indent rule"
        (is (empty? missing-break)
            (str "Symbols in indent-rules but not force-break-rules: "
                 (vec missing-break))))
      (testing "has an indent rule for every force-break rule"
        (is (empty? missing-indent)
            (str "Symbols in force-break-rules but not indent-rules: "
                 (vec missing-indent)))))))

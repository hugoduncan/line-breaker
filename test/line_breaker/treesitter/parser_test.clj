(ns line-sitter.treesitter.parser-test
  "Tests for parser wrapper functions.

  Tests verify:
  - Parsing valid Clojure source returns a Tree
  - Parsing source with syntax errors returns a tree with ERROR nodes
  - Parsing empty string returns a valid tree"
  (:require [clojure.test :refer [deftest is testing]]
            [line-sitter.treesitter.parser :as parser])
  (:import [io.github.treesitter.jtreesitter Tree Node]))

(deftest parse-source-test
  ;; Verify parse-source handles various inputs correctly.
  ;; Tree-sitter returns a tree even for invalid syntax, with ERROR nodes.
  (testing "parse-source"
    (testing "parses valid Clojure source"
      (let [source "(defn foo [x] (+ x 1))"
            tree (parser/parse-source source)]
        (is (instance? Tree tree)
            "returns a Tree instance")
        (is (instance? Node (.getRootNode tree))
            "tree has a root node")
        (is (not (.hasError (.getRootNode tree)))
            "valid source has no error nodes")))

    (testing "parses source with syntax errors"
      ;; Tree-sitter still returns a tree, marking errors with ERROR nodes
      (let [source "(defn foo [x"  ; unclosed brackets
            tree (parser/parse-source source)]
        (is (instance? Tree tree)
            "returns a Tree instance even for invalid syntax")
        (is (.hasError (.getRootNode tree))
            "root node indicates errors in tree")))

    (testing "parses empty string"
      (let [tree (parser/parse-source "")]
        (is (instance? Tree tree)
            "returns a Tree instance for empty input")
        (is (instance? Node (.getRootNode tree))
            "empty input has a root node")))))

(deftest with-parser-test
  ;; Verify with-parser macro provides a configured parser.
  (testing "with-parser"
    (testing "provides a parser with Clojure language set"
      (parser/with-parser [p]
        (is (some? (.getLanguage p))
            "parser has language configured")))

    (testing "allows parsing within body"
      (let [result (parser/with-parser [p]
                     (let [optional-tree (.parse p "(+ 1 2)")]
                       (when (.isPresent optional-tree)
                         (.get optional-tree))))]
        (is (instance? Tree result)
            "can parse within with-parser body")))))

(ns line-breaker.treesitter.node-test
  "Tests for node access helper functions.

  Tests traverse a parsed (defn foo [x] x) form to verify:
  - Node types are returned as keywords
  - Text extraction works correctly
  - Position and range information is accurate
  - Child enumeration works for both all and named-only"
  (:require [clojure.test :refer [deftest is testing]]
            [line-breaker.treesitter.node :as node]
            [line-breaker.treesitter.parser :as parser])
  (:import [io.github.treesitter.jtreesitter Node]))

(deftest root-node-test
  ;; Verify root-node extracts the root node from a Tree.
  (testing "root-node"
    (testing "returns the root Node from a Tree"
      (let [tree (parser/parse-source "(defn foo [x] x)")
            root (node/root-node tree)]
        (is (instance? Node root)
            "returns a Node instance")))

    (testing "returns nil for nil input"
      (is (nil? (node/root-node nil))
          "nil tree returns nil"))))

(deftest node-type-test
  ;; Verify node-type returns grammar rule names as keywords.
  (testing "node-type"
    (testing "returns the grammar rule name as a keyword"
      (let [tree (parser/parse-source "(+ 1 2)")
            root (node/root-node tree)]
        (is (= :source (node/node-type root))
            "root node type is :source")))

    (testing "returns correct types for nested nodes"
      (let [tree (parser/parse-source "(+ 1 2)")
            root (node/root-node tree)
            list-node (first (node/named-children root))]
        (is (= :list_lit (node/node-type list-node))
            "list expression is :list_lit")))

    (testing "returns nil for nil input"
      (is (nil? (node/node-type nil))
          "nil node returns nil"))))

(deftest node-text-test
  ;; Verify node-text returns the source text spanning a node.
  (testing "node-text"
    (testing "returns the source text for a node"
      (let [tree (parser/parse-source "(defn foo [x] x)")
            root (node/root-node tree)]
        (is (= "(defn foo [x] x)" (node/node-text root))
            "root contains full source")))

    (testing "returns text for nested nodes"
      (let [tree (parser/parse-source "(defn foo [x] x)")
            root (node/root-node tree)
            defn-form (first (node/named-children root))
            children (node/named-children defn-form)]
        (is (= "defn" (node/node-text (first children)))
            "first child is defn symbol")
        (is (= "foo" (node/node-text (second children)))
            "second child is foo symbol")))

    (testing "returns nil for nil input"
      (is (nil? (node/node-text nil))
          "nil node returns nil"))))

(deftest node-range-test
  ;; Verify node-range returns byte offsets.
  (testing "node-range"
    (testing "returns [start-byte end-byte] vector"
      (let [tree (parser/parse-source "(+ 1 2)")
            root (node/root-node tree)]
        (is (= [0 7] (node/node-range root))
            "root spans entire source")))

    (testing "returns correct ranges for nested nodes"
      (let [tree (parser/parse-source "(+ 1 2)")
            root (node/root-node tree)
            list-node (first (node/named-children root))
            plus-sym (first (node/named-children list-node))]
        (is (= [1 2] (node/node-range plus-sym))
            "+ symbol is at bytes 1-2")))

    (testing "returns nil for nil input"
      (is (nil? (node/node-range nil))
          "nil node returns nil"))))

(deftest node-position-test
  ;; Verify node-position returns row/column map.
  (testing "node-position"
    (testing "returns {:row :column} map"
      (let [tree (parser/parse-source "(+ 1 2)")
            root (node/root-node tree)]
        (is (= {:row 0 :column 0} (node/node-position root))
            "root starts at 0,0")))

    (testing "returns correct positions for multiline"
      (let [tree (parser/parse-source "(foo\n  bar)")
            root (node/root-node tree)
            list-node (first (node/named-children root))
            bar-sym (second (node/named-children list-node))]
        (is (= {:row 1 :column 2} (node/node-position bar-sym))
            "bar is on line 1, column 2")))

    (testing "returns nil for nil input"
      (is (nil? (node/node-position nil))
          "nil node returns nil"))))

(deftest children-test
  ;; Verify children returns all child nodes including anonymous ones.
  (testing "children"
    (testing "returns all child nodes"
      (let [tree (parser/parse-source "(+ 1 2)")
            root (node/root-node tree)
            list-node (first (node/named-children root))
            all-children (node/children list-node)]
        ;; Should include ( + 1 2 ) - parens and content
        (is (seq all-children)
            "has children")
        (is (> (count all-children) 3)
            "includes anonymous nodes like parens")))

    (testing "returns nil for nil input"
      (is (nil? (node/children nil))
          "nil node returns nil"))))

(deftest named-children-test
  ;; Verify named-children returns only named nodes.
  (testing "named-children"
    (testing "returns only named children"
      (let [tree (parser/parse-source "(+ 1 2)")
            root (node/root-node tree)
            list-node (first (node/named-children root))
            named (node/named-children list-node)]
        (is (= 3 (count named))
            "three named children: +, 1, 2")
        (is (= ["+" "1" "2"] (mapv node/node-text named))
            "named children are the symbols/numbers")))

    (testing "excludes punctuation"
      (let [tree (parser/parse-source "[a b]")
            root (node/root-node tree)
            vec-node (first (node/named-children root))
            named (node/named-children vec-node)]
        ;; Should not include [ or ]
        (is (= 2 (count named))
            "only a and b, not brackets")
        (is (= ["a" "b"] (mapv node/node-text named))
            "named children are the symbols")))

    (testing "returns nil for nil input"
      (is (nil? (node/named-children nil))
          "nil node returns nil"))))

(deftest next-named-sibling-test
  ;; Verify next-named-sibling returns the next named sibling node.
  (testing "next-named-sibling"
    (testing "returns next named sibling"
      (let [tree (parser/parse-source "(a) (b) (c)")
            root (node/root-node tree)
            children (node/named-children root)
            first-child (first children)]
        (is (= "(b)" (node/node-text (node/next-named-sibling first-child)))
            "next sibling of (a) is (b)")))

    (testing "returns nil for last sibling"
      (let [tree (parser/parse-source "(a) (b)")
            root (node/root-node tree)
            last-child (last (node/named-children root))]
        (is (nil? (node/next-named-sibling last-child))
            "last sibling has no next")))

    (testing "returns nil for nil input"
      (is (nil? (node/next-named-sibling nil))
          "nil node returns nil"))))

(deftest node-line-range-test
  ;; Verify node-line-range returns 1-indexed line range.
  (testing "node-line-range"
    (testing "returns [start end] for single line"
      (let [tree (parser/parse-source "(foo bar)")
            root (node/root-node tree)]
        (is (= [1 1] (node/node-line-range root))
            "single line is [1 1]")))

    (testing "returns correct range for multiline"
      (let [tree (parser/parse-source "(foo\n  bar\n  baz)")
            root (node/root-node tree)]
        (is (= [1 3] (node/node-line-range root))
            "three lines is [1 3]")))

    (testing "returns correct range for nested node"
      (let [tree (parser/parse-source "(a)\n(b\n  c)")
            root (node/root-node tree)
            second-form (second (node/named-children root))]
        (is (= [2 3] (node/node-line-range second-form))
            "second form spans lines 2-3")))

    (testing "returns nil for nil input"
      (is (nil? (node/node-line-range nil))
          "nil node returns nil"))))

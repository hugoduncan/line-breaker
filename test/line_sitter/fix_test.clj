(ns line-sitter.fix-test
  "Tests for line breaking functions.

  Tests cover:
  - Edit application in correct order
  - Breakable node type detection
  - Finding outermost breakable forms on a line
  - Generating break edits for various collection types"
  (:require
   [clojure.test :refer [deftest is testing]]
   [line-sitter.fix :as fix]
   [line-sitter.treesitter.node :as node]
   [line-sitter.treesitter.parser :as parser]))

(deftest apply-edits-test
  ;; Verify edits are applied correctly in reverse start order.
  (testing "apply-edits"
    (testing "replaces text at the specified range"
      (is (= "helloXworld"
             (fix/apply-edits "hello world"
                              [{:start 5 :end 6 :replacement "X"}]))))

    (testing "applies multiple edits in reverse start order"
      (is (= "aXcYe"
             (fix/apply-edits "abcde"
                              [{:start 1 :end 2 :replacement "X"}
                               {:start 3 :end 4 :replacement "Y"}]))))

    (testing "handles empty edit list"
      (is (= "unchanged"
             (fix/apply-edits "unchanged" []))))

    (testing "handles replacement at start"
      (is (= "Xello"
             (fix/apply-edits "hello"
                              [{:start 0 :end 1 :replacement "X"}]))))

    (testing "handles insertion when start equals end"
      (is (= "helloX"
             (fix/apply-edits "hello"
                              [{:start 5 :end 5 :replacement "X"}]))))))

(deftest breakable-node?-test
  ;; Verify breakable node detection for collection types.
  (testing "breakable-node?"
    (testing "returns true for list_lit"
      (let [tree (parser/parse-source "(a b)")
            root (node/root-node tree)
            list-node (first (node/named-children root))]
        (is (fix/breakable-node? list-node))))

    (testing "returns true for vec_lit"
      (let [tree (parser/parse-source "[a b]")
            root (node/root-node tree)
            vec-node (first (node/named-children root))]
        (is (fix/breakable-node? vec-node))))

    (testing "returns true for map_lit"
      (let [tree (parser/parse-source "{:a 1}")
            root (node/root-node tree)
            map-node (first (node/named-children root))]
        (is (fix/breakable-node? map-node))))

    (testing "returns true for set_lit"
      (let [tree (parser/parse-source "#{a b}")
            root (node/root-node tree)
            set-node (first (node/named-children root))]
        (is (fix/breakable-node? set-node))))

    (testing "returns false for sym_lit"
      (let [tree (parser/parse-source "foo")
            root (node/root-node tree)
            sym-node (first (node/named-children root))]
        (is (not (fix/breakable-node? sym-node)))))

    (testing "returns false for str_lit"
      (let [tree (parser/parse-source "\"hello\"")
            root (node/root-node tree)
            str-node (first (node/named-children root))]
        (is (not (fix/breakable-node? str-node)))))))

(deftest find-breakable-form-test
  ;; Verify finding the outermost breakable form on a line.
  (testing "find-breakable-form"
    (testing "finds simple list on line 1"
      (let [tree (parser/parse-source "(a b c)")
            form (fix/find-breakable-form tree 1)]
        (is (some? form))
        (is (= :list_lit (node/node-type form)))
        (is (= "(a b c)" (node/node-text form)))))

    (testing "finds outermost form when nested"
      (let [tree (parser/parse-source "(a (b c) d)")
            form (fix/find-breakable-form tree 1)]
        (is (= "(a (b c) d)" (node/node-text form))
            "returns outer form, not inner")))

    (testing "returns nil for line without breakable form"
      (let [tree (parser/parse-source "foo")
            form (fix/find-breakable-form tree 1)]
        (is (nil? form))))

    (testing "finds form on correct line in multiline source"
      (let [tree (parser/parse-source "(a)\n(b c d)")
            form (fix/find-breakable-form tree 2)]
        (is (= "(b c d)" (node/node-text form)))))

    (testing "returns nil for empty line"
      (let [tree (parser/parse-source "(a)\n\n(b)")
            form (fix/find-breakable-form tree 2)]
        (is (nil? form))))))

(deftest break-form-test
  ;; Verify edit generation for breaking forms.
  (testing "break-form"
    (testing "generates edits for simple list"
      (let [tree (parser/parse-source "(a b c)")
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)
            result (fix/apply-edits "(a b c)" edits)]
        (is (= "(a\n  b\n  c)" result))))

    (testing "generates edits for vector"
      (let [tree (parser/parse-source "[a b c]")
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)
            result (fix/apply-edits "[a b c]" edits)]
        (is (= "[a\n  b\n  c]" result))))

    (testing "generates edits for map"
      (let [tree (parser/parse-source "{:a 1 :b 2}")
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)
            result (fix/apply-edits "{:a 1 :b 2}" edits)]
        (is (= "{:a\n  1\n  :b\n  2}" result))))

    (testing "returns nil for single-element form"
      (let [tree (parser/parse-source "(a)")
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)]
        (is (nil? edits))))

    (testing "preserves indentation based on form position"
      (let [source "  (a b c)"
            tree (parser/parse-source source)
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)
            result (fix/apply-edits source edits)]
        (is (= "  (a\n    b\n    c)" result)
            "indentation accounts for form's column position")))))

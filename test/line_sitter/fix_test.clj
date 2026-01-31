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

(deftest find-long-lines-test
  ;; Verify detection of lines exceeding max length.
  (testing "find-long-lines"
    (testing "returns empty vector when all lines fit"
      (is (= [] (fix/find-long-lines "short" 10))))

    (testing "returns line number for single long line"
      (is (= [1] (fix/find-long-lines "this is too long" 10))))

    (testing "returns multiple line numbers"
      (is (= [1 3] (fix/find-long-lines "long line here\nok\nlong line here" 10))))

    (testing "handles empty string"
      (is (= [] (fix/find-long-lines "" 10))))

    (testing "uses 1-indexed line numbers"
      (is (= [2] (fix/find-long-lines "ok\nthis is too long\nok" 10))))))

(deftest fix-source-test
  ;; Verify the iterative multi-pass breaking loop.
  (testing "fix-source"
    (testing "returns source unchanged when within limit"
      (is (= "(a b c)" (fix/fix-source "(a b c)" {:line-length 80}))))

    (testing "breaks simple form in one pass"
      (is (= "(a\n  b\n  c)"
             (fix/fix-source "(a b c)" {:line-length 5}))))

    (testing "handles nested forms requiring multiple passes"
      ;; First pass breaks outer, second pass breaks inner
      (let [source "(a (b c d e) f)"
            result (fix/fix-source source {:line-length 10})]
        (is (= "(a\n  (b\n    c\n    d\n    e)\n  f)" result))))

    (testing "leaves unbreakable atoms unchanged"
      ;; Long string cannot be broken - :def rule keeps name on first line
      (let [source "(def x \"long-string\")"
            result (fix/fix-source source {:line-length 10})]
        ;; Can break the form but not the string
        (is (= "(def x\n  \"long-string\")" result))))

    (testing "handles already broken source"
      (let [source "(a\n  b\n  c)"]
        (is (= source (fix/fix-source source {:line-length 80})))))

    (testing "uses default line-length of 80"
      (let [short-source "(a b c)"
            long-source (str "(" (apply str (repeat 40 "a ")) ")")]
        (is (= short-source (fix/fix-source short-source {})))
        (is (not= long-source (fix/fix-source long-source {})))))))

(deftest indent-rules-test
  ;; Verify that :defn and :def indent rules keep name on first line.
  (testing "indent rules"
    (testing "for defn"
      (testing "keeps name on first line"
        (let [source "(defn foo [x] (+ x 1))"
              result (fix/fix-source source {:line-length 15})]
          (is (= "(defn foo\n  [x]\n  (+ x 1))" result)))))

    (testing "for defn-"
      (testing "keeps name on first line"
        (let [source "(defn- bar [x] x)"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(defn- bar\n  [x]\n  x)" result)))))

    (testing "for defmacro"
      (testing "keeps name on first line"
        (let [source "(defmacro m [x] `(do ~x))"
              result (fix/fix-source source {:line-length 15})]
          (is (= "(defmacro m\n  [x]\n  `(do ~x))" result)))))

    (testing "for def"
      (testing "keeps name on first line"
        (let [source "(def myvar 42)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(def myvar\n  42)" result)))))

    (testing "for defonce"
      (testing "keeps name on first line"
        (let [source "(defonce state {})"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(defonce state\n  {})" result)))))

    (testing "for defmulti"
      (testing "keeps name on first line"
        (let [source "(defmulti dispatch type)"
              result (fix/fix-source source {:line-length 15})]
          (is (= "(defmulti dispatch\n  type)" result)))))

    (testing "for custom indent rule"
      (testing "uses config :indents"
        (let [source "(my-defn foo [x] x)"
              config {:line-length 12 :indents {'my-defn :defn}}
              result (fix/fix-source source config)]
          (is (= "(my-defn foo\n  [x]\n  x)" result)))))

    (testing "for fn"
      (testing "keeps arg vector on first line"
        (let [source "(fn [x] (+ x 1))"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(fn [x]\n  (+ x 1))" result)))))

    (testing "for bound-fn"
      (testing "keeps arg vector on first line"
        (let [source "(bound-fn [x] x)"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(bound-fn [x]\n  x)" result)))))

    (testing "for let"
      (testing "keeps binding vector on first line"
        (let [source "(let [x 1] (+ x 2))"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(let [x 1]\n  (+ x 2))" result)))))

    (testing "for when-let"
      (testing "keeps binding vector on first line"
        (let [source "(when-let [x y] body)"
              result (fix/fix-source source {:line-length 15})]
          (is (= "(when-let [x y]\n  body)" result)))))

    (testing "for if-let"
      (testing "keeps binding vector on first line"
        (let [source "(if-let [x y] a b)"
              result (fix/fix-source source {:line-length 14})]
          (is (= "(if-let [x y]\n  a\n  b)" result)))))

    (testing "for doseq"
      (testing "keeps binding vector on first line"
        (let [source "(doseq [x xs] (prn x))"
              result (fix/fix-source source {:line-length 14})]
          (is (= "(doseq [x xs]\n  (prn x))" result)))))

    (testing "for for"
      (testing "keeps binding vector on first line"
        (let [source "(for [x xs] (* x 2))"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(for [x xs]\n  (* x 2))" result)))))

    (testing "for loop"
      (testing "keeps binding vector on first line"
        (let [source "(loop [i 0] (recur i))"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(loop [i 0]\n  (recur i))" result)))))

    (testing "for non-special forms"
      (testing "breaks all elements"
        (let [source "(foo bar baz)"
              result (fix/fix-source source {:line-length 8})]
          (is (= "(foo\n  bar\n  baz)" result)))))))

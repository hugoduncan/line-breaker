(ns line-breaker.fix-test
  "Tests for line breaking functions.

  Tests cover:
  - Edit application in correct order
  - Breakable node type detection
  - Finding outermost breakable forms on a line
  - Generating break edits for various collection types
  - Ignore mechanism integration"
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [line-breaker.fix :as fix]
   [line-breaker.treesitter.node :as node]
   [line-breaker.treesitter.parser :as parser]))

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
        (is (nil? form))))

    (testing "with metadata"
      (testing "returns metadata-wrapped form as breakable"
        ;; The vec_lit "^double [[a b] [c d]]" has a meta_lit child.
        ;; It IS breakable (with special indent handling), but we don't
        ;; descend into its children.
        (let [source "^double [[a b] [c d]]"
              tree (parser/parse-source source)
              forms (fix/find-breakable-forms tree 1)
              form-texts (mapv node/node-text forms)]
          (is (= [source] form-texts)
              "metadata-wrapped form is breakable")))

      (testing "does not return nodes inside metadata-annotated form"
        ;; In a defn, the arg vector may have metadata attached.
        ;; The inner vectors [a b] and [c d] should not be returned.
        (let [source "(defn foo ^ret [[a b] [c d]] body)"
              tree (parser/parse-source source)
              forms (fix/find-breakable-forms tree 1)
              form-texts (map node/node-text forms)]
          (is (not (some #{"[a b]" "[c d]"} form-texts))
              "inner vectors in metadata-annotated form not returned")
          (is (some #{source} form-texts)
              "outer defn form is returned"))))))

(deftest break-form-test
  ;; Verify edit generation for breaking forms.
  (testing "break-form"
    (testing "generates edits for simple list"
      (let [tree (parser/parse-source "(a b c)")
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)
            result (fix/apply-edits "(a b c)" edits)]
        (is (= "(a\n b\n c)" result))))

    (testing "generates edits for vector"
      (let [tree (parser/parse-source "[a b c]")
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)
            result (fix/apply-edits "[a b c]" edits)]
        (is (= "[a\n b\n c]" result))))

    (testing "generates edits for map with pair grouping"
      (let [tree (parser/parse-source "{:a 1 :b 2}")
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)
            result (fix/apply-edits "{:a 1 :b 2}" edits)]
        (is (= "{:a 1\n  :b 2}" result))))

    (testing "returns nil for single-element form"
      (let [tree (parser/parse-source "(a)")
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)]
        (is (nil? edits))))

    (testing "returns nil for metadata-only form (no content element)"
      ;; Edge case: ^double [] has metadata but the inner vec is empty.
      ;; tree-sitter parses this as vec_lit with only meta_lit as a child.
      ;; break-form should return nil since there's no content to break.
      (let [tree (parser/parse-source "^double []")
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)]
        (is (nil? edits))))

    (testing "preserves indentation based on form position"
      (let [source "  (a b c)"
            tree (parser/parse-source source)
            form (fix/find-breakable-form tree 1)
            edits (fix/break-form form)
            result (fix/apply-edits source edits)]
        (is (= "  (a\n   b\n   c)" result)
            "indentation accounts for form's column position")))))

(deftest find-long-lines-test
  ;; Verify detection of lines exceeding max length.
  (testing "find-long-lines"
    (testing "returns empty vector when all lines fit"
      (is (= [] (fix/find-long-lines "short" 10))))

    (testing "returns line number for single long line"
      (is (= [1] (fix/find-long-lines "this is too long" 10))))

    (testing "returns multiple line numbers"
      (is
       (=
        [1 3]
        (fix/find-long-lines "long line here\nok\nlong line here" 10))))

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
      (is (= "(a\n b\n c)"
             (fix/fix-source "(a b c)" {:line-length 5}))))

    (testing "handles nested forms requiring multiple passes"
      ;; With 1-space indent, inner form fits on line after outer breaks
      (let [source "(a (b c d e) f)"
            result (fix/fix-source source {:line-length 10})]
        (is (= "(a\n (b c d e)\n f)" result))))

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

    (testing "for if"
      (testing "keeps test on first line"
        (let [source "(if test then else)"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(if test\n  then\n  else)" result)))))

    (testing "for if-not"
      (testing "keeps test on first line"
        (let [source "(if-not test a b)"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(if-not test\n  a\n  b)" result)))))

    (testing "for when"
      (testing "keeps test on first line"
        (let [source "(when test body1 body2)"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(when test\n  body1\n  body2)" result)))))

    (testing "for when-not"
      (testing "keeps test on first line"
        (let [source "(when-not test body)"
              result (fix/fix-source source {:line-length 15})]
          (is (= "(when-not test\n  body)" result)))))

    (testing "for when-first"
      (testing "keeps binding on first line"
        (let [source "(when-first [x xs] body)"
              result (fix/fix-source source {:line-length 18})]
          (is (= "(when-first [x xs]\n  body)" result)))))

    (testing "for case"
      (testing "keeps test-expr on first line with pair grouping"
        (let [source "(case x :a 1 :b 2)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(case x\n  :a 1\n  :b 2)" result)))))

    (testing "for cond"
      (testing "uses pair grouping for test-result clauses"
        (let [source "(cond t1 r1 t2 r2)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(cond\n  t1 r1\n  t2 r2)" result)))))

    (testing "for condp"
      (testing "keeps pred and expr on first line with pair grouping"
        (let [source "(condp = x :a 1 :b 2)"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(condp = x\n  :a 1\n  :b 2)" result)))))

    (testing "for cond->"
      (testing "keeps initial expr on first line with pair grouping"
        (let [source "(cond-> x t1 f1 t2 f2)"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(cond-> x\n  t1 f1\n  t2 f2)" result)))))

    (testing "for try"
      (testing "puts body on next line"
        (let [source "(try expr catch)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(try\n  expr\n  catch)" result))))
      (testing "handles multiple body expressions"
        (let [source "(try a b c d)"
              result (fix/fix-source source {:line-length 8})]
          (is (= "(try\n  a\n  b\n  c\n  d)" result)))))

    (testing "for do"
      (testing "puts body on next line"
        (let [source "(do expr1 expr2)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(do\n  expr1\n  expr2)" result))))
      (testing "handles single body expression"
        (let [source "(do expr)"
              result (fix/fix-source source {:line-length 5})]
          (is (= "(do\n  expr)" result)))))

    (testing "for custom indent rule via config"
      (testing "applies :try rule to custom form"
        (let [source "(my-try a b c)"
              config {:line-length 8 :indents {'my-try :try}}
              result (fix/fix-source source config)]
          (is (= "(my-try\n  a\n  b\n  c)" result))))
      (testing "applies :do rule to custom form"
        (let [source "(my-do a b)"
              config {:line-length 6 :indents {'my-do :do}}
              result (fix/fix-source source config)]
          (is (= "(my-do\n  a\n  b)" result)))))

    (testing "for non-special forms"
      (testing "breaks all elements"
        (let [source "(foo bar baz)"
              result (fix/fix-source source {:line-length 8})]
          (is (= "(foo\n bar\n baz)" result)))))))

(deftest ignore-mechanism-test
  ;; Verify that #_:line-breaker/ignore prevents breaking of marked forms.
  (testing "ignore mechanism integration"
    (testing "does not break ignored form"
      (let [source "#_:line-breaker/ignore (foo bar baz qux)"
            result (fix/fix-source source {:line-length 10})]
        (is (= source result)
            "ignored form stays on single line")))

    (testing "does not break nested form inside ignored"
      (let [source "#_:line-breaker/ignore (foo (bar baz qux))"
            result (fix/fix-source source {:line-length 10})]
        (is (= source result)
            "nested forms within ignored are also preserved")))

    (testing "breaks non-ignored forms"
      (let [source "(foo bar baz) #_:line-breaker/ignore (keep this)"
            result (fix/fix-source source {:line-length 10})]
        (is (= "(foo\n bar\n baz) #_:line-breaker/ignore (keep this)" result)
            "non-ignored form is broken, ignored form is preserved")))

    (testing "preserves ignore marker in output"
      (let [source "#_:line-breaker/ignore (long form here)"
            result (fix/fix-source source {:line-length 10})]
        (is (str/includes? result "#_:line-breaker/ignore")
            "ignore marker is preserved")))))

(deftest reader-macro-test
  ;; Verify reader macros stay attached and breakable forms are handled.
  (testing "reader macros"
    (testing "for anonymous functions"
      (testing "breaks when exceeding limit"
        (let [source "#(foo a b c d)"
              result (fix/fix-source source {:line-length 8})]
          (is (= "#(foo\n a\n b\n c\n d)" result))))
      (testing "stays attached when inside list"
        (let [source "(map #(+ % 1) xs)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(map\n #(+ % 1)\n xs)" result)))))

    (testing "for reader conditionals"
      (testing "breaks when exceeding limit"
        (let [source "#?(:clj a :cljs b)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "#?(:clj\n a\n :cljs\n b)" result))))
      (testing "splicing variant breaks"
        (let [source "#?@(:clj [a] :cljs [b])"
              result (fix/fix-source source {:line-length 12})]
          (is (= "#?@(:clj\n [a]\n :cljs\n [b])" result)))))

    (testing "for quote"
      (testing "stays attached to following form"
        (let [source "(foo 'bar baz qux)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(foo\n 'bar\n baz\n qux)" result))))
      (testing "inner list breaks correctly"
        (let [source "'(a b c d e)"
              result (fix/fix-source source {:line-length 6})]
          (is (= "'(a\n  b\n  c\n  d\n  e)" result)))))

    (testing "for syntax-quote"
      (testing "stays attached to following form"
        (let [source "(foo `bar baz qux)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(foo\n `bar\n baz\n qux)" result)))))

    (testing "for unquote"
      (testing "stays attached to following form"
        (let [source "`(foo ~bar baz)"
              result (fix/fix-source source {:line-length 8})]
          (is (= "`(foo\n  ~bar\n  baz)" result)))))

    (testing "for unquote-splicing"
      (testing "stays attached to following form"
        (let [source "`(foo ~@bar baz)"
              result (fix/fix-source source {:line-length 8})]
          (is (= "`(foo\n  ~@bar\n  baz)" result)))))

    (testing "for deref"
      (testing "stays attached to following form"
        (let [source "(foo @atom bar baz)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(foo\n @atom\n bar\n baz)" result)))))

    (testing "for var-quote"
      (testing "stays attached to following form"
        (let [source "(foo #'var bar baz)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(foo\n #'var\n bar\n baz)" result)))))

    (testing "for metadata"
      (testing "stays attached to following form"
        (let [source "(foo ^:key bar baz)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(foo\n ^:key bar\n baz)" result))))

      (testing "breaks metadata-wrapped vector with proper alignment"
        ;; Metadata stays on first line with first content element.
        ;; Subsequent elements align with the first content element.
        (let [source "^double [[a] [b] [c]]"
              result (fix/fix-source source {:line-length 15})]
          (is (= "^double [[a]\n         [b]\n         [c]]" result))))

      (testing "does not break metadata-wrapped vector when line fits"
        ;; When line length is sufficient, form should not be broken.
        (let [source "^double [[a] [b]]"
              result (fix/fix-source source {:line-length 30})]
          (is (= "^double [[a] [b]]" result))))

      (testing "aligns with short metadata prefix"
        ;; Shorter metadata ^ret vs ^double affects indent position.
        (let [source "^ret [[a b] [c d] [e f]]"
              result (fix/fix-source source {:line-length 15})]
          (is (= "^ret [[a b]\n      [c d]\n      [e f]]" result))))

      (testing "breaks type-hinted defn arg vector correctly"
        ;; The bug case from story 142: type hint stays attached,
        ;; destructuring vectors align properly.
        (let [source (str "(defn- f \"doc\" "
                          "^ret [[a b] [c d] [e f]] body)")
              result (fix/fix-source source {:line-length 20})]
          (is (= (str "(defn- f\n  \"doc\"\n"
                      "  ^ret [[a b]\n"
                      "        [c d]\n"
                      "        [e f]]\n"
                      "  body)")
                 result))))

      (testing "preserves metadata-wrapped form when outer form breaks"
        ;; When defn breaks but type-hinted arg fits, keep it intact.
        (let [source "(defn foo ^double [[x y] [a b]] body)"
              result (fix/fix-source source {:line-length 30})]
          (is (= (str "(defn foo\n"
                      "  ^double [[x y] [a b]]\n"
                      "  body)")
                 result)))))))

(deftest comment-handling-test
  ;; Verify inline comments stay attached and don't cause extra blank lines.
  (testing "comment handling"
    (testing "keeps end-of-line comment attached to preceding element"
      (let [source "(foo a ; comment\nb c)"
            result (fix/fix-source source {:line-length 10})]
        (is (= "(foo\n a ; comment\n b\n c)" result))))

    (testing "keeps comment attached to head element"
      (let [source "(foo ; after head\na b c)"
            result (fix/fix-source source {:line-length 10})]
        (is (= "(foo ; after head\n a\n b\n c)" result))))

    (testing "handles multiple comments"
      (let [source "(foo a ; one\nb ; two\nc)"
            result (fix/fix-source source {:line-length 10})]
        (is (= "(foo\n a ; one\n b ; two\n c)" result))))

    (testing "does not introduce extra blank lines"
      (let [source "(foo a ; comment\nb)"
            result (fix/fix-source source {:line-length 10})]
        (is (not (str/includes? result "\n\n"))
            "no double newlines in output")))

    (testing "preserves comment content"
      (let [source "(foo a ; important note\nb)"
            result (fix/fix-source source {:line-length 10})]
        (is (str/includes? result "; important note")
            "comment text is preserved")))))

(deftest pair-grouping-test
  ;; Verify pair grouping for maps, cond, case, and related forms.
  (testing "pair grouping"
    (testing "for maps"
      (testing "keeps key-value pairs together"
        (let [source "{:a 1 :b 2 :c 3}"
              result (fix/fix-source source {:line-length 10})]
          (is (= "{:a 1\n  :b 2\n  :c 3}" result))))
      (testing "handles nested values"
        (let [source "{:a [1 2] :b [3 4]}"
              result (fix/fix-source source {:line-length 12})]
          (is (= "{:a [1 2]\n  :b [3 4]}" result))))
      (testing "leaves oversized pairs with atomic values together"
        ;; Pairs with atomic values stay together even if they exceed line
        ;; length, since splitting wouldn't help reduce line width.
        (let [source "{:a 1 :longkey longval}"
              result (fix/fix-source source {:line-length 12})]
          (is (= "{:a 1\n  :longkey longval}" result)))))

    (testing "for case"
      (testing "handles default clause (odd element count)"
        (let [source "(case x :a 1 :b 2 :default)"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(case x\n  :a 1\n  :b 2\n  :default)" result))))
      (testing "keeps test-expr on first line"
        (let [source "(case expr :a 1)"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(case expr\n  :a 1)" result)))))

    (testing "for cond"
      (testing "handles multiple pairs"
        (let [source "(cond a 1 b 2 c 3)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(cond\n  a 1\n  b 2\n  c 3)" result))))
      (testing "handles :else clause"
        (let [source "(cond a 1 :else 2)"
              result (fix/fix-source source {:line-length 10})]
          (is (= "(cond\n  a 1\n  :else 2)" result)))))

    (testing "for condp"
      (testing "keeps predicate and expr on first line"
        (let [source "(condp = val :a 1 :b 2)"
              result (fix/fix-source source {:line-length 14})]
          (is (= "(condp = val\n  :a 1\n  :b 2)" result))))
      (testing "handles default clause"
        (let [source "(condp = x :a 1 :default)"
              result (fix/fix-source source {:line-length 12})]
          (is (= "(condp = x\n  :a 1\n  :default)" result)))))

    (testing "for cond->"
      (testing "keeps initial expr on first line"
        (let [source "(cond-> val t1 f1 t2 f2)"
              result (fix/fix-source source {:line-length 14})]
          (is (= "(cond-> val\n  t1 f1\n  t2 f2)" result)))))

    (testing "for cond->>"
      (testing "keeps initial expr on first line"
        (let [source "(cond->> val t1 f1 t2 f2)"
              result (fix/fix-source source {:line-length 14})]
          (is (= "(cond->> val\n  t1 f1\n  t2 f2)" result)))))))

(deftest binding-vector-pair-grouping-test
  ;; Verify binding vectors use pair grouping when broken.
  (testing "binding vector pair grouping"
    (testing "for let"
      (testing "keeps symbol-value pairs together"
        (let [source "(let [x 1 y 2 z 3] body)"
              result (fix/fix-source source {:line-length 14})]
          (is (= "(let [x 1\n      y 2\n      z 3]\n  body)" result))))
      (testing "aligns to bracket position"
        (let [source "  (let [a 1 b 2] x)"
              result (fix/fix-source source {:line-length 14})]
          (is (= "  (let [a 1\n        b 2]\n    x)" result)))))

    (testing "for loop"
      (testing "keeps symbol-value pairs together"
        (let [source "(loop [i 0 n 10] (recur i n))"
              result (fix/fix-source source {:line-length 14})]
          (is (= "(loop [i 0\n       n 10]\n  (recur i n))" result)))))

    (testing "for doseq"
      (testing "keeps binding pairs together"
        (let [source "(doseq [x xs y ys] (prn x y))"
              result (fix/fix-source source {:line-length 16})]
          (is (= "(doseq [x xs\n        y ys]\n  (prn x y))" result)))))

    (testing "for for"
      (testing "keeps binding pairs together"
        (let [source "(for [x xs y ys] [x y])"
              result (fix/fix-source source {:line-length 14})]
          (is (= "(for [x xs\n      y ys]\n  [x y])" result)))))

    (testing "standalone vectors"
      (testing "do not use pair grouping"
        (let [source "[a 1 b 2 c 3]"
              result (fix/fix-source source {:line-length 8})]
          (is (= "[a\n 1\n b\n 2\n c\n 3]" result)
              "standalone vectors break each element"))))

    (testing "nested bindings"
      (testing "each binding vector grouped independently"
        (let [source "(let [x (let [a 1 b 2] a)] x)"
              result (fix/fix-source source {:line-length 18})]
          (is (str/includes? result "[a 1\n")
              "inner binding vector uses pair grouping"))))

    (testing "destructuring"
      (testing "keeps pattern-value pairs together"
        (let [source "(let [{:keys [a]} m x 1] body)"
              result (fix/fix-source source {:line-length 18})]
          (is (= "(let [{:keys [a]} m\n      x 1]\n  body)" result)))))

    (testing "with comments"
      (testing "does not let comment disrupt pair grouping"
        ;; Bug 150: Comments in binding vectors were counted in pair grouping,
        ;; causing binding pairs to be incorrectly split.
        (let [source (str "(let [a 1\n"
                          "      ;; comment\n"
                          "      b (f x y z)\n"
                          "      c 3]\n"
                          "  x)")
              result (fix/fix-source source {:line-length 80})]
          (is (str/includes? result "b (f x y z)")
              "binding pair stays together")
          (is (str/includes? result "c 3")
              "subsequent pair unaffected")))

      (testing "breaks value when needed despite preceding comment"
        ;; The value should be broken, not the pair split
        (let [source (str "(let [a 1\n"
                          "      ;; comment\n"
                          "      b (long-fn arg1 arg2 arg3)]\n"
                          "  x)")
              result (fix/fix-source source {:line-length 40})]
          (is (not (re-find #"b\n\s+\(long" result))
              "pair not split (name not orphaned)")
          (is (str/includes? result "(long-fn")
              "value present")))

      (testing "preserves comment between pairs"
        (let [source (str "(let [a 1\n"
                          "      ;; middle comment\n"
                          "      b 2]\n"
                          "  x)")
              result (fix/fix-source source {:line-length 80})]
          (is (str/includes? result ";; middle comment")
              "comment preserved"))))))

(deftest nested-binding-value-test
  ;; Verify forms nested inside binding pair values are properly broken.
  ;; Bug 79: Deeply nested forms in binding values weren't auto-broken.
  (testing "nested binding value forms"
    (testing "breaks function call in let binding value"
      ;; Inner (mapcat ...) call should break when it exceeds line length
      (let [source "(let [result (mapcat f (get-items))] result)"
            result (fix/fix-source source {:line-length 35})]
        (is (< (apply max (map count (str/split-lines result))) 36)
            "all lines should be within limit")
        (is (str/includes? result "(mapcat")
            "mapcat call should be present")))

    (testing "breaks deeply nested form in catch"
      ;; The (throw (ex-info ...)) pattern from bug report
      (let [source "(try x (catch E e (throw (ex-info \"err\" {:k v}))))"
            result (fix/fix-source source {:line-length 40})]
        (is (< (apply max (map count (str/split-lines result))) 41)
            "all lines should be within limit")))

    (testing "breaks form inside binding value with multiple bindings"
      ;; When multiple bindings, form in later binding should still break
      (let [source "(let [x 1 y (some-long-function a b c d)] body)"
            result (fix/fix-source source {:line-length 30})]
        (is (< (apply max (map count (str/split-lines result))) 31)
            "all lines should be within limit")))))

(deftest value-expression-breaking-test
  ;; Bug 152: Value expressions in pair-grouped forms were not being broken
  ;; when they exceeded the line limit. When a key-value pair is too long
  ;; and the value is a breakable form, the pair should be split with the
  ;; key on one line and the value on the next.
  (testing "value expression breaking"
    (testing "for maps"
      (testing "splits pair when value is breakable and pair exceeds limit"
        (let [source "{:some-long-key (fn-call a b c)}"
              result (fix/fix-source source {:line-length 25})]
          (is (= "{:some-long-key\n  (fn-call a b c)}" result))))

      (testing "breaks value after split when value still exceeds limit"
        (let [source "{:some-long-key (fn-call a b c d e)}"
              result (fix/fix-source source {:line-length 20})]
          (is (str/includes? result ":some-long-key\n")
              "key and value are split")
          (is (< (apply max (map count (str/split-lines result))) 21)
              "all lines within limit")))

      (testing "keeps atomic values with keys even when exceeding limit"
        ;; Splitting a pair with an atomic value doesn't help
        (let [source "{:some-long-key atomic-val}"
              result (fix/fix-source source {:line-length 20})]
          (is (= source result)
              "atomic value stays with key")))

      (testing "handles multiple pairs with first pair too long"
        (let [source "{:key1 (long-fn a b) :key2 val2}"
              result (fix/fix-source source {:line-length 15})]
          (is (str/includes? result ":key1\n")
              "first pair is split")
          (is (str/includes? result ":key2")
              "second pair is present"))))

    (testing "for cond"
      (testing "splits test-result pair when result is breakable"
        (let [source "(cond (test?) (long-fn a b c) :else x)"
              result (fix/fix-source source {:line-length 20})]
          (is (< (apply max (map count (str/split-lines result))) 21)
              "all lines within limit"))))

    (testing "for binding vectors"
      (testing "splits binding pair when value is breakable"
        (let [source "(let [x (long-fn a b c d e)] body)"
              result (fix/fix-source source {:line-length 20})]
          (is (str/includes? result "x\n")
              "binding name and value are split")
          (is (< (apply max (map count (str/split-lines result))) 21)
              "all lines within limit")))

      (testing "keeps atomic binding values with names"
        (let [source "(let [some-long-name atomic-val] body)"
              result (fix/fix-source source {:line-length 25})]
          (is (str/includes? result "some-long-name atomic-val")
              "atomic value stays with binding name"))))))

(deftest edge-cases-test
  ;; Verify edge cases: empty, single-element, already-formatted.
  (testing "edge cases"
    (testing "for empty collections"
      (testing "leaves empty list unchanged"
        (is (= "()" (fix/fix-source "()" {:line-length 1}))))
      (testing "leaves empty vector unchanged"
        (is (= "[]" (fix/fix-source "[]" {:line-length 1}))))
      (testing "leaves empty map unchanged"
        (is (= "{}" (fix/fix-source "{}" {:line-length 1}))))
      (testing "leaves empty set unchanged"
        (is (= "#{}" (fix/fix-source "#{}" {:line-length 1})))))

    (testing "for single-element collections"
      (testing "leaves single-element list unchanged"
        (is (= "(a)" (fix/fix-source "(a)" {:line-length 2}))))
      (testing "leaves single-element vector unchanged"
        (is (= "[x]" (fix/fix-source "[x]" {:line-length 2}))))
      (testing "leaves map with one pair unchanged when within limit"
        ;; Map with k v has 2 children, stays unchanged if within limit
        (is (= "{k v}" (fix/fix-source "{k v}" {:line-length 10})))))

    (testing "for already-formatted code"
      (testing "leaves properly broken list unchanged"
        (let [source "(a\n  b\n  c)"]
          (is (= source (fix/fix-source source {:line-length 80})))))
      (testing "leaves properly broken vector unchanged"
        (let [source "[a\n b\n c]"]
          (is (= source (fix/fix-source source {:line-length 80})))))
      (testing "leaves properly broken defn unchanged"
        (let [source "(defn foo\n  [x]\n  (+ x 1))"]
          (is (= source (fix/fix-source source {:line-length 80}))))))))

(deftest atomic-literal-integrity-test
  ;; Verify atomic literals (strings, etc.) are never broken internally.
  ;; Breaks should occur between form elements, not inside literals.
  ;; Tests the contract that strings remain intact after line breaking.
  (testing "multiple short strings exceeding line length"
    (testing "breaks between strings, not inside them"
      ;; Four 5-char strings in a vector: each fits, but total exceeds 40 chars
      (let [source "(vector \"abc\" \"def\" \"ghi\" \"jkl\")"
            result (fix/fix-source source {:line-length 20})]
        (is (str/includes? result "\"abc\"")
            "first string remains intact")
        (is (str/includes? result "\"def\"")
            "second string remains intact")
        (is (str/includes? result "\"ghi\"")
            "third string remains intact")
        (is (str/includes? result "\"jkl\"")
            "fourth string remains intact")))

    (testing "handles strings at beginning position"
      (let [source "(\"first\" a b c)"
            result (fix/fix-source source {:line-length 10})]
        (is (str/includes? result "\"first\"")
            "string at beginning remains intact")))

    (testing "handles strings at middle position"
      (let [source "(a \"mid\" b c)"
            result (fix/fix-source source {:line-length 8})]
        (is (str/includes? result "\"mid\"")
            "string in middle remains intact")))

    (testing "handles strings at end position"
      (let [source "(a b c \"last\")"
            result (fix/fix-source source {:line-length 8})]
        (is (str/includes? result "\"last\"")
            "string at end remains intact")))

    (testing "handles mixed strings and other elements"
      (let [source "(fn \"s1\" :k1 \"s2\" sym)"
            result (fix/fix-source source {:line-length 12})]
        (is (str/includes? result "\"s1\"")
            "first string remains intact")
        (is (str/includes? result "\"s2\"")
            "second string remains intact"))))

  (testing "strings with special characters"
    (testing "preserves escaped quotes within strings"
      (let [source "(f \"say \\\"hi\\\"\" x)"
            result (fix/fix-source source {:line-length 10})]
        (is (str/includes? result "\"say \\\"hi\\\"\"")
            "string with escaped quotes remains intact")))

    (testing "preserves escaped newlines within strings"
      (let [source "(f \"line1\\nline2\" x)"
            result (fix/fix-source source {:line-length 10})]
        (is (str/includes? result "\"line1\\nline2\"")
            "string with escaped newline remains intact")))

    (testing "preserves escaped tabs within strings"
      (let [source "(f \"col1\\tcol2\" x)"
            result (fix/fix-source source {:line-length 10})]
        (is (str/includes? result "\"col1\\tcol2\"")
            "string with escaped tab remains intact")))

    (testing "preserves multiple escape sequences"
      (let [source "(f \"a\\\"b\\nc\\td\" x)"
            result (fix/fix-source source {:line-length 10})]
        (is (str/includes? result "\"a\\\"b\\nc\\td\"")
            "string with mixed escapes remains intact"))))

  (testing "regex literals"
    (testing "preserves simple regex pattern"
      (let [source "(re-find #\"abc\" s)"
            result (fix/fix-source source {:line-length 12})]
        (is (str/includes? result "#\"abc\"")
            "simple regex remains intact")))

    (testing "preserves regex with quantifiers"
      (let [source "(re-find #\"\\d+\" text)"
            result (fix/fix-source source {:line-length 12})]
        (is (str/includes? result "#\"\\d+\"")
            "regex with quantifier remains intact")))

    (testing "preserves regex with alternation"
      (let [source "(re-find #\"a|b\" text)"
            result (fix/fix-source source {:line-length 12})]
        (is (str/includes? result "#\"a|b\"")
            "regex with alternation remains intact")))

    (testing "handles multiple regex in form"
      (let [source "(or (re-find #\"foo\" x) (re-find #\"bar\" x))"
            result (fix/fix-source source {:line-length 25})]
        (is (str/includes? result "#\"foo\"")
            "first regex remains intact")
        (is (str/includes? result "#\"bar\"")
            "second regex remains intact"))))

  (testing "character literals"
    (testing "preserves simple character literal"
      (let [source "(conj chars \\a \\b \\c)"
            result (fix/fix-source source {:line-length 12})]
        (is (str/includes? result "\\a")
            "character \\a remains intact")
        (is (str/includes? result "\\b")
            "character \\b remains intact")
        (is (str/includes? result "\\c")
            "character \\c remains intact")))

    (testing "preserves named character literals"
      (let [source "(list \\newline \\space)"
            result (fix/fix-source source {:line-length 14})]
        (is (str/includes? result "\\newline")
            "newline character remains intact")
        (is (str/includes? result "\\space")
            "space character remains intact")))

    (testing "handles mixed character and string literals"
      (let [source "(vector \\a \"str\" \\b)"
            result (fix/fix-source source {:line-length 12})]
        (is (str/includes? result "\\a")
            "character literal remains intact")
        (is (str/includes? result "\"str\"")
            "string literal remains intact")
        (is (str/includes? result "\\b")
            "second character literal remains intact"))))

  (testing "line length boundary conditions"
    (testing "string ending exactly at line limit"
      ;; "(x \"abcd\")" is exactly 10 chars - stays intact at limit 10
      (let [source "(x \"abcd\")"
            result (fix/fix-source source {:line-length 10})]
        (is (= source result)
            "form fitting exactly at limit stays on one line")))

    (testing "string starting one char before limit"
      ;; "(abcdefg \"s\")" is 13 chars; at limit 10, break is needed
      ;; The string starts near the boundary
      (let [source "(abcdefg \"s\")"
            result (fix/fix-source source {:line-length 10})]
        (is (str/includes? result "\"s\"")
            "string remains intact even when starting near boundary")))

    (testing "multiple small atoms fitting exactly on limit"
      ;; "[a b c d]" is exactly 9 chars - at limit 9, no break needed
      (let [source "[a b c d]"
            result (fix/fix-source source {:line-length 9})]
        (is (= source result)
            "atoms fitting exactly at limit stay on one line")))

    (testing "multiple strings totaling exactly line limit"
      ;; "(\"ab\" \"cd\")" is exactly 12 chars
      (let [source "(\"ab\" \"cd\")"
            result (fix/fix-source source {:line-length 12})]
        (is (= source result)
            "strings totaling exactly limit stay on one line")))

    (testing "mixed atoms at exact boundary with keyword"
      ;; "(:k \"ab\" 1)" is exactly 11 chars
      (let [source "(:k \"ab\" 1)"
            result (fix/fix-source source {:line-length 11})]
        (is (= source result)
            "mixed atoms at exact limit stay on one line")))

    (testing "one char over boundary forces break"
      ;; "(:k \"abc\" 1)" is 12 chars; at limit 11 needs break
      (let [source "(:k \"abc\" 1)"
            result (fix/fix-source source {:line-length 11})]
        (is (not= source result)
            "one char over limit triggers break")
        (is (str/includes? result "\"abc\"")
            "string remains intact after break")))

    (testing "string at boundary with symbol"
      ;; Form where string would be broken if not atomic
      (let [source "(sym \"boundary\")"
            result (fix/fix-source source {:line-length 10})]
        (is (str/includes? result "\"boundary\"")
            "string near boundary remains intact")))

    (testing "regex at exact boundary"
      ;; "(re #\"ab\")" is exactly 10 chars
      (let [source "(re #\"ab\")"
            result (fix/fix-source source {:line-length 10})]
        (is (= source result)
            "regex fitting exactly at limit stays on one line")))

    (testing "character literal at boundary"
      ;; "(f \\a \\b)" is exactly 9 chars
      (let [source "(f \\a \\b)"
            result (fix/fix-source source {:line-length 9})]
        (is (= source result)
            "character literals at exact limit stay on one line")))))

(deftest unicode-handling-test
  ;; Verify multi-byte UTF-8 characters don't cause corruption.
  ;; Tree-sitter returns byte offsets, but Clojure strings use char indices.
  ;; Bug 141: Without byte-to-char conversion, edits corrupt multi-byte chars.
  (testing "unicode handling"
    (testing "preserves multi-byte characters when breaking"
      ;; é is 2 bytes in UTF-8
      (let [source "(é b c)"
            result (fix/fix-source source {:line-length 5})]
        (is (= "(é\n b\n c)" result)
            "2-byte UTF-8 char preserved")))

    (testing "preserves 3-byte characters"
      ;; → is 3 bytes in UTF-8
      (let [source "(→ b c)"
            result (fix/fix-source source {:line-length 5})]
        (is (= "(→\n b\n c)" result)
            "3-byte UTF-8 char preserved")))

    (testing "preserves 4-byte characters"
      ;; 😀 is 4 bytes in UTF-8
      (let [source "(😀 b c)"
            result (fix/fix-source source {:line-length 5})]
        (is (= "(😀\n b\n c)" result)
            "4-byte UTF-8 char preserved")))

    (testing "handles mixed ASCII and multi-byte"
      (let [source "(foo ébar baz)"
            result (fix/fix-source source {:line-length 8})]
        (is (str/includes? result "ébar")
            "symbol with multi-byte char preserved")))

    (testing "handles unicode in string literals"
      (let [source "(f \"café\" x)"
            result (fix/fix-source source {:line-length 8})]
        (is (str/includes? result "\"café\"")
            "string with multi-byte char preserved")))

    (testing "handles multiple multi-byte chars"
      (let [source "(é ü ñ)"
            result (fix/fix-source source {:line-length 5})]
        (is (str/includes? result "é")
            "first multi-byte char preserved")
        (is (str/includes? result "ü")
            "second multi-byte char preserved")
        (is (str/includes? result "ñ")
            "third multi-byte char preserved")))))

(deftest type-hinted-forms-test
  ;; Verify metadata/type hints stay attached to their annotated forms.
  ;; Story 142: Type hints were incorrectly separated from arg vectors.
  (testing "for type-hinted forms"
    (testing "fn with type-hinted arg vector"
      (testing "keeps type hint attached when breaking"
        (let [source "(fn ^long [^int x ^int y] (+ x y))"
              result (fix/fix-source source {:line-length 20})]
          (is (str/includes? result "^long [")
              "type hint stays attached to arg vector")
          (is (not (str/includes? result "^long\n"))
              "type hint not orphaned on separate line"))))

    (testing "letfn with type-hinted functions"
      (testing "keeps type hints attached in nested fn"
        (let [source "(letfn [(f ^long [^int x] x)] (f 1))"
              result (fix/fix-source source {:line-length 25})]
          (is (str/includes? result "^long [")
              "type hint stays attached in letfn function"))))

    (testing "let binding with type hint"
      (testing "keeps type hint attached to binding symbol"
        (let [source "(let [^String s \"foo\"] s)"
              result (fix/fix-source source {:line-length 20})]
          (is (str/includes? result "^String s")
              "type hint stays attached to binding symbol"))))

    (testing "loop binding with type hint"
      (testing "keeps type hint attached to binding symbol"
        (let [source "(loop [^int i 0 ^int n 10] (recur (inc i) n))"
              result (fix/fix-source source {:line-length 25})]
          (is (str/includes? result "^int i")
              "first type hint stays attached")
          (is (str/includes? result "^int n")
              "second type hint stays attached"))))

    (testing "multiple metadata on same element"
      (testing "keeps all metadata attached"
        (let [source "(def ^:private ^:deprecated legacy-fn (fn [] nil))"
              result (fix/fix-source source {:line-length 30})]
          (is (str/includes? result "^:private ^:deprecated")
              "multiple metadata items stay together")
          (is (str/includes? result "^:deprecated legacy-fn")
              "metadata stays attached to symbol"))))))

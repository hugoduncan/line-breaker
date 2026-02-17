(ns line-breaker.reformat-test
  "Tests for reformat functions.

  Tests cover:
  - Collapsing top-level forms to single lines
  - Forced line break insertion
  - Pair breaking for pair-grouped forms
  - Full reformat pipeline (collapse, force-break, re-break)
  - Comment indentation preservation through reformat
  - No rightward drift in nested forms"
  (:require
   [clojure.test :refer [deftest is testing]]
   [line-breaker.reformat :as reformat]))

(deftest collapse-top-level-forms-test
  ;; Verify collapsing all top-level forms to single lines.
  ;; collapse-top-level-forms recursively collapses each top-level form,
  ;; preserving comment newlines and multi-line string content.
  (testing "collapse-top-level-forms"
    (testing "collapses a multi-line form to single line"
      (is
       (=
        "(defn foo [x] (+ x 1))"
        (reformat/collapse-top-level-forms
         "(defn foo\n  [x]\n  (+ x 1))"))))
    (testing "collapses deeply nested multi-line forms"
      (is
       (=
        "(defn foo [x] (+ x 1))"
        (reformat/collapse-top-level-forms
         "(defn foo\n  [x]\n  (+\n    x\n    1))"))))
    (testing "preserves EOL comment"
      (is
       (=
        "(defn foo [x] ;; the arg\n (+ x 1))"
        (reformat/collapse-top-level-forms
         "(defn foo\n  [x] ;; the arg\n  (+ x 1))"))))
    (testing "preserves whole-line comment leading newline"
      (is
       (=
        "(defn foo\n;; add one\n [x] (+ x 1))"
        (reformat/collapse-top-level-forms
         "(defn foo\n  ;; add one\n  [x]\n  (+ x 1))"))))
    (testing "leaves multi-line string content untouched"
      (is
       (=
        "(def sql \"SELECT *\n   FROM users\")"
        (reformat/collapse-top-level-forms
         "(def sql\n  \"SELECT *\n   FROM users\")"))))
    (testing "collapses multiple top-level forms independently"
      (is
       (=
        "(defn foo [x] x)\n\n(defn bar [y] y)"
        (reformat/collapse-top-level-forms
         (str
          "(defn foo\n  [x]\n  x)"
          "\n\n"
          "(defn bar\n  [y]\n  y)")))))
    (testing "leaves already single-line form unchanged"
      (is
       (=
        "(+ 1 2)"
        (reformat/collapse-top-level-forms "(+ 1 2)"))))
    (testing "collapses ignored forms"
      ;; Ignore directives are not respected during collapse
      (is
       (=
        "#_:line-breaker/ignore\n(defn foo [x] (+ x 1))"
        (reformat/collapse-top-level-forms
         (str
          "#_:line-breaker/ignore\n"
          "(defn foo\n  [x]\n  (+ x 1))")))))))

(deftest apply-forced-breaks-test
  ;; Verify forced line breaks at structurally significant positions.
  ;; apply-forced-breaks inserts breaks after specific child indices
  ;; and after specific child types in matching forms.
  (testing "apply-forced-breaks"
    (testing "given a defn"
      (testing "breaks after name and argvec"
        (is
         (=
          "(defn foo\n  [x]\n  (+ x 1))"
          (reformat/apply-forced-breaks
           "(defn foo [x] (+ x 1))"
           {})))))
    (testing "given a defn with docstring"
      (testing "breaks after name, after docstring, and after argvec"
        (is
         (=
          (str
           "(defn foo\n"
           "  \"doc\"\n"
           "  [x]\n"
           "  (+ x 1))")
          (reformat/apply-forced-breaks
           "(defn foo \"doc\" [x] (+ x 1))"
           {})))))
    (testing "given a multi-arity defn"
      (testing "breaks after name and after argvec in each arity"
        (is
         (=
          (str
           "(defn foo\n"
           "  ([x]\n"
           "   x)\n"
           "  ([x y]\n"
           "   y))")
          (reformat/apply-forced-breaks
           "(defn foo ([x] x) ([x y] y))"
           {})))))
    (testing "given a multi-arity fn"
      (testing "breaks after argvec in each arity"
        (is
         (=
          (str
           "(fn\n"
           "  ([x]\n"
           "   x)\n"
           "  ([x y]\n"
           "   y))")
          (reformat/apply-forced-breaks
           "(fn ([x] x) ([x y] y))"
           {})))))
    (testing "given a multi-arity defn with docstring"
      (testing "breaks after name, docstring, and argvec in each arity"
        (is
         (=
          (str
           "(defn foo\n"
           "  \"doc\"\n"
           "  ([x]\n"
           "   x)\n"
           "  ([x y]\n"
           "   y))")
          (reformat/apply-forced-breaks
           "(defn foo \"doc\" ([x] x) ([x y] y))"
           {})))))
    (testing "given a defn with metadata on name"
      (testing "breaks after metadata-wrapped name and argvec"
        (is
         (=
          "(defn ^:private foo\n  [x]\n  x)"
          (reformat/apply-forced-breaks
           "(defn ^:private foo [x] x)"
           {})))))
    (testing "given a defmethod"
      (testing "breaks after dispatch-val and argvec"
        (is
         (=
          "(defmethod foo :bar\n  [x]\n  x)"
          (reformat/apply-forced-breaks
           "(defmethod foo :bar [x] x)"
           {})))))
    (testing "given a deftest"
      (testing "breaks after name"
        (is
         (=
          "(deftest my-test\n  (is (= 1 1)))"
          (reformat/apply-forced-breaks
           "(deftest my-test (is (= 1 1)))"
           {})))))
    (testing "given a ns"
      (testing "breaks after name"
        (is
         (=
          (str
           "(ns my.ns\n"
           "  (:require\n"
           "   [foo]))")
          (reformat/apply-forced-breaks
           "(ns my.ns (:require [foo]))"
           {}))))
      (testing "breaks after docstring"
        (is
         (=
          (str
           "(ns my.ns\n"
           "  \"A namespace.\"\n"
           "  (:require\n"
           "   [foo]))")
          (reformat/apply-forced-breaks
           "(ns my.ns \"A namespace.\" (:require [foo]))"
           {}))))
      (testing "breaks each require libspec onto its own line"
        (is
         (=
          (str
           "(ns my.ns\n"
           "  (:require\n"
           "   [a]\n"
           "   [b]\n"
           "   [c]))")
          (reformat/apply-forced-breaks
           "(ns my.ns (:require [a] [b] [c]))"
           {}))))
      (testing "breaks symbol libspecs"
        (is
         (=
          (str
           "(ns my.ns\n"
           "  (:require\n"
           "   clojure.string\n"
           "   clojure.set))")
          (reformat/apply-forced-breaks
           "(ns my.ns (:require clojure.string clojure.set))"
           {}))))
      (testing "breaks import children onto own lines"
        (is
         (=
          (str
           "(ns my.ns\n"
           "  (:import\n"
           "   [java.io File]\n"
           "   [java.util Map]))")
          (reformat/apply-forced-breaks
           "(ns my.ns (:import [java.io File] [java.util Map]))"
           {}))))
      (testing "breaks single-child require after keyword"
        (is
         (=
          (str
           "(ns my.ns\n"
           "  (:require\n"
           "   [a]))")
          (reformat/apply-forced-breaks
           "(ns my.ns (:require [a]))"
           {}))))
      (testing "breaks between require and import clauses"
        (is
         (=
          (str
           "(ns my.ns\n"
           "  (:require\n"
           "   [a])\n"
           "  (:import\n"
           "   [java.io File]))")
          (reformat/apply-forced-breaks
           "(ns my.ns (:require [a]) (:import [java.io File]))"
           {})))))
    (testing "given a def"
      (testing "breaks after name"
        (is
         (=
          "(def foo\n  42)"
          (reformat/apply-forced-breaks "(def foo 42)" {}))))
      (testing "breaks after docstring"
        (is
         (=
          "(def foo\n  \"A var.\"\n  42)"
          (reformat/apply-forced-breaks
           "(def foo \"A var.\" 42)"
           {})))))
    (testing "given a defonce"
      (testing "breaks after name"
        (is
         (=
          "(defonce foo\n  42)"
          (reformat/apply-forced-breaks
           "(defonce foo 42)"
           {})))))
    (testing "given a defmulti"
      (testing "breaks after name"
        (is
         (=
          "(defmulti foo\n  :type)"
          (reformat/apply-forced-breaks
           "(defmulti foo :type)"
           {})))))
    (testing "given nested forms"
      (testing "breaks outer before inner"
        (is
         (=
          "(def foo\n  (defn bar\n    [x]\n    x))"
          (reformat/apply-forced-breaks
           "(def foo (defn bar [x] x))"
           {})))))
    (testing "given a defn with multiple body forms"
      (testing "breaks between body siblings"
        (is
         (=
          "(defn foo\n  [x]\n  (bar x)\n  (baz x))"
          (reformat/apply-forced-breaks
           "(defn foo [x] (bar x) (baz x))"
           {})))))
    (testing "given an already-broken form"
      (testing "returns unchanged"
        (let [s "(defn foo\n  [x]\n  (+ x 1))"]
          (is
           (= s (reformat/apply-forced-breaks s {}))))))
    (testing "given a do with multiple body forms"
      (testing "breaks between every body form"
        (is
         (=
          "(do\n  (a)\n  (b)\n  (c))"
          (reformat/apply-forced-breaks
           "(do (a) (b) (c))"
           {})))))
    (testing "given a do with two body forms"
      (testing "breaks between them"
        (is
         (=
          "(do\n  (a)\n  (b))"
          (reformat/apply-forced-breaks
           "(do (a) (b))"
           {})))))
    (testing "given a do with one body form"
      (testing "only breaks after do keyword"
        (is
         (=
          "(do\n  (a))"
          (reformat/apply-forced-breaks
           "(do (a))"
           {})))))
    (testing "with user config override"
      (testing "uses config :force-breaks over defaults"
        (is
         (=
          "(defn foo [x] (+ x 1))"
          (reformat/apply-forced-breaks
           "(defn foo [x] (+ x 1))"
           {:force-breaks {'defn {}}})))))))

(deftest reformat-source-test
  ;; Verify the three-pass collapse, force-break, then re-break approach.
  ;; reformat-source first collapses all forms, then inserts forced
  ;; breaks, then applies fix-source to re-break exceeding lines.
  (testing "reformat-source"
    (testing "re-breaks poorly broken form correctly"
      (is
       (=
        "(defn foo\n  [x]\n  (+ x 1))"
        (reformat/reformat-source
         "(defn foo\n  [x]\n  (+ x\n    1))"
         {:line-length 40}))))
    (testing "applies forced breaks even within limit"
      (is
       (=
        "(defn foo\n  [x]\n  (+ x 1))"
        (reformat/reformat-source
         "(defn foo\n  [x]\n  (+ x 1))"
         {:line-length 40}))))
    (testing "collapses then re-breaks when exceeding limit"
      (is
       (=
        "(defn my-function\n  [x y]\n  (+ x y (- x y)))"
        (reformat/reformat-source
         (str
          "(defn my-function [x y]\n"
          "  (+ x y\n"
          "    (- x y)))")
         {:line-length 30}))))
    (testing "preserves EOL comment through collapse and re-break"
      (is
       (=
        (str
         "(defn my-longer-fn\n"
         "  [x] ;; arg\n"
         "  (+\n"
         "   x\n"
         "   (very-long-computation x)))")
        (reformat/reformat-source
         (str
          "(defn my-longer-fn\n"
          "  [x] ;; arg\n"
          "  (+ x\n"
          "    (very-long-computation x)))")
         {:line-length 30}))))
    (testing "preserves whole-line comment through collapse and re-break"
      (is
       (=
        (str
         "(defn my-longer-fn\n"
         "  ;; does computation\n"
         "  [x]\n"
         "  (+\n"
         "   x\n"
         "   (very-long-computation x)))")
        (reformat/reformat-source
         (str
          "(defn my-longer-fn\n"
          "  ;; does computation\n"
          "  [x]\n"
          "  (+ x\n"
          "    (very-long-computation x)))")
         {:line-length 30}))))
    (testing "reformats ns require onto separate lines"
      (is
       (=
        (str
         "(ns my.ns\n"
         "  (:require\n"
         "   [a]\n"
         "   [b]\n"
         "   [c]))")
        (reformat/reformat-source
         (str
          "(ns my.ns\n"
          "  (:require [a] [b]\n"
          "            [c]))")
         {:line-length 80}))))
    (testing "reformats ns import onto separate lines"
      (is
       (=
        (str
         "(ns my.ns\n"
         "  (:import\n"
         "   [java.io File]))")
        (reformat/reformat-source
         "(ns my.ns (:import [java.io File]))"
         {:line-length 80}))))
    (testing "breaks do body forms onto separate lines"
      (is
       (=
        "(do\n  (a)\n  (b)\n  (c))"
        (reformat/reformat-source
         "(do (a)\n  (b) (c))"
         {:line-length 80}))))
    (testing "breaks do body forms in map values"
      (is
       (=
        (str
         "{:task (do\n"
         "         (a)\n"
         "         (b))}")
        (reformat/reformat-source
         "{:task (do (a) (b))}"
         {:line-length 80}))))
    (testing "breaks when-not body in let with comment"
      (is
       (=
        (str
         "(let [x 1]\n"
         "  ;; check x\n"
         "  (when-not x\n"
         "    (throw (ex-info \"err\" {}))))")
        (reformat/reformat-source
         (str
          "(let [x 1]\n"
          "  ;; check x\n"
          "  (when-not x (throw (ex-info \"err\" {}))))")
         {:line-length 80}))))))

(deftest apply-pair-breaking-test
  ;; Verify that apply-pair-breaking forces pair-grouped forms onto
  ;; separate lines even when they fit within the line length.
  ;; Single-pair forms are left on one line.
  (testing "apply-pair-breaking"
    (testing "given a map with multiple pairs"
      (testing "breaks each pair onto its own line"
        (is
         (=
          "{:a 1\n :b 2\n :c 3}"
          (reformat/apply-pair-breaking
           "{:a 1 :b 2 :c 3}"
           {})))))
    (testing "given a map with one pair"
      (testing "leaves it on one line"
        (is
         (=
          "{:a 1}"
          (reformat/apply-pair-breaking "{:a 1}" {})))))
    (testing "given a cond with multiple clauses"
      (testing "breaks each clause onto its own line"
        (is
         (=
          (str
           "(cond\n"
           "  (= x 1) :one\n"
           "  (= x 2) :two\n"
           "  :else :other)")
          (reformat/apply-pair-breaking
           (str
            "(cond (= x 1) :one"
            " (= x 2) :two"
            " :else :other)")
           {})))))
    (testing "given a cond with one clause"
      (testing "leaves it on one line"
        (is
         (=
          "(cond (= x 1) :one)"
          (reformat/apply-pair-breaking
           "(cond (= x 1) :one)"
           {})))))
    (testing "given a condp with multiple clauses"
      (testing "breaks each clause onto its own line"
        (is
         (=
          (str "(condp = x\n" "  1 :one\n" "  2 :two)")
          (reformat/apply-pair-breaking
           "(condp = x 1 :one 2 :two)"
           {})))))
    (testing "given a case with multiple clauses"
      (testing "breaks each clause onto its own line"
        (is
         (=
          (str "(case x\n" "  1 :one\n" "  2 :two)")
          (reformat/apply-pair-breaking
           "(case x 1 :one 2 :two)"
           {})))))
    (testing "given a cond-> with multiple clauses"
      (testing "breaks each clause onto its own line"
        (is
         (=
          (str
           "(cond-> x\n"
           "  true inc\n"
           "  false dec)")
          (reformat/apply-pair-breaking
           "(cond-> x true inc false dec)"
           {})))))
    (testing "given a binding vector"
      (testing "breaks each pair onto its own line"
        (is
         (=
          "(let [a 1\n      b 2] (+ a b))"
          (reformat/apply-pair-breaking
           "(let [a 1 b 2] (+ a b))"
           {})))))
    (testing "given nested maps"
      (testing "breaks both outer and inner maps"
        (is
         (=
          "{:a {:x 1\n     :y 2}\n :b 3}"
          (reformat/apply-pair-breaking
           "{:a {:x 1 :y 2} :b 3}"
           {})))))))

(deftest reformat-source-pair-breaking-test
  ;; Verify that reformat-source forces pair-grouped forms to break
  ;; even when they fit within the line length after collapse.
  (testing "reformat-source with pair breaking"
    (testing "given a map that fits on one line"
      (testing "forces pair breaking"
        (is
         (=
          "{:a 1\n :b 2\n :c 3}"
          (reformat/reformat-source
           "{:a 1 :b 2 :c 3}"
           {:line-length 80})))))
    (testing "given a defn containing a map"
      (testing "forces map pair breaking"
        (is
         (=
          (str
           "(defn foo\n"
           "  []\n"
           "  {:a 1\n"
           "   :b 2})")
          (reformat/reformat-source
           "(defn foo [] {:a 1 :b 2})"
           {:line-length 80})))))
    (testing "given a poorly broken cond"
      (testing "collapses then pair-breaks"
        (is
         (=
          (str
           "(cond\n"
           "  (= x 1) :one\n"
           "  (= x 2) :two)")
          (reformat/reformat-source
           (str
            "(cond (= x 1)\n"
            "  :one (= x 2)\n"
            "  :two)")
           {:line-length 80})))))
    (testing "given a cond with long test+value pairs"
      (testing "keeps pair together when broken value head fits"
        (let [input (str
                     "(cond (not (fs/exists? p))"
                     " (throw (ex-info \"m\" {}))"
                     " :else nil)")
              result (reformat/reformat-source
                      input
                      {:line-length 40})]
          (is
           (re-find
            #"(?m)\(not \(fs/exists\? p\)\) \(throw"
            result)
           "test and value head on same line"))))))

(deftest reformat-comment-indentation-test
  ;; Verify that whole-line comments inside pair-grouped forms retain
  ;; correct indentation after collapse and re-break, and that forms
  ;; following comment chains in forced-break positions are re-indented.
  (testing "reformat-source"
    (testing "given whole-line comments inside cond branches"
      (testing "indents comments to cond body indent"
        (let [input (str
                     "(cond\n"
                     "  (= :x rule)\n"
                     "  ;; First comment.\n"
                     "  ;; Second comment.\n"
                     "  (if-let [c (f n)]\n"
                     "    (g c)\n"
                     "    (+ 1 b))\n"
                     "  (some? r) (+ 2 b)\n"
                     "  :else (+ 1 b))")
              result (reformat/reformat-source
                      input
                      {:line-length 40})]
          (is
           (re-find #"(?m)^  ;; First comment\." result)
           "first comment at column 2")
          (is
           (re-find #"(?m)^  ;; Second comment\." result)
           "second comment at column 2")
          (is
           (re-find #"(?m)^  \(if-let " result)
           "if-let at column 2"))))
    (testing "given a defn with comment before argvec"
      (testing "indents comment and argvec to body indent"
        (let [input (str
                     "(defn foo\n"
                     "  ;; doc comment\n"
                     "  [x]\n"
                     "  (+ x 1))")
              result (reformat/reformat-source
                      input
                      {:line-length 40})]
          (is
           (re-find #"(?m)^  ;; doc comment" result)
           "comment at column 2")
          (is
           (re-find #"(?m)^  \[x\]" result)
           "argvec at column 2"))))
    (testing "given a defn body comment between regular forms"
      (testing "indents comment to body indent"
        (let [input (str
                     "(defn uber\n"
                     "  \"Build.\"\n"
                     "  [_]\n"
                     "  (clean nil)\n"
                     "  (javac nil)\n"
                     "  ;; Copy resources\n"
                     "  (b/copy-dir {:src [\"r\"]})\n"
                     "  (b/copy-dir {:src [\"s\"]}))")
              result (reformat/reformat-source
                      input
                      {:line-length 80})]
          (is
           (re-find #"(?m)^  ;; Copy resources" result)
           "comment at column 2")
          (is
           (re-find #"(?m)^  \(b/copy-dir" result)
           "form after comment at column 2"))))))

(deftest reformat-no-rightward-drift-test
  ;; When fix-source processes multiple long lines in one pass, a
  ;; parent form (e.g. let) on line N and a child form (e.g. reduce)
  ;; on line N+1 could both be broken. The child's indent must be
  ;; computed from its position AFTER the parent's edits move it, not
  ;; from its pre-edit column on the collapsed line.
  (testing "reformat-source"
    (testing "given nested forms on a long collapsed line"
      (testing "does not drift child indentation rightward"
        (let [input (str
                     "(defn process\n"
                     "  [source edits]\n"
                     "  (let [sorted (sort-by :s > edits)]\n"
                     "    (doseq [[h l] (partition 2 1 sorted)]\n"
                     "      (when (> (:end l) (:start h))\n"
                     "        (throw (ex-info \"err\""
                     " {:a l :b h}))))\n"
                     "    (reduce\n"
                     "     (fn [s {:keys [start end rep]}]\n"
                     "       (str (subs s 0 start)"
                     " rep (subs s end)))\n"
                     "     source\n"
                     "     sorted)))")
              result (reformat/reformat-source
                      input
                      {:line-length 40})]
          ;; reduce's children must be at column 5 (reduce-col + 1),
          ;; not at a high column from the collapsed line
          (is
           (every? #(<= (count %) 40) (.split result "\n"))
           "no line exceeds line-length")
          ;; Check reduce's fn arg is properly indented
          (is
           (re-find #"(?m)^ {5}\(fn " result)
           "fn arg indented at col 5 (reduce col+1)"))))
    (testing "given deeply nested let with body forms on a single line"
      (testing "breaks body forms at correct indentation"
        (let [input (str
                     "(defn run\n"
                     "  [x]\n"
                     "  (let [a (compute x)]\n"
                     "    (when (pos? a)\n"
                     "      (println a))\n"
                     "    (transform a x)))")
              result (reformat/reformat-source
                      input
                      {:line-length 30})]
          ;; Verify no extreme indentation
          (is
           (every? #(<= (count %) 30) (.split result "\n"))
           "no line exceeds line-length"))))
    (testing "given a form that was already correct"
      (testing "is idempotent"
        (let [input (str
                     "(defn foo\n"
                     "  [x y]\n"
                     "  (let [a (bar x)]\n"
                     "    (baz a y)))")
              result (reformat/reformat-source
                      input
                      {:line-length 40})]
          (is
           (=
            result
            (reformat/reformat-source result {:line-length 40}))
           "second reformat produces same output"))))))

(deftest intermediate-ancestor-reformat-test
  ;; When nested forms share a line and the outermost ancestor is
  ;; already broken, intermediate ancestors should be tried so inner
  ;; forms get separated onto their own lines.
  (testing "reformat-source"
    (testing "given nested testing forms on one line"
      (testing "separates at intermediate ancestor"
        (let [input (str
                     "(deftest my-test\n"
                     "  (testing \"outer\""
                     " (testing \"inner\"\n"
                     "    (let [x (long-fn a b)]"
                     " (do-thing x)))))")
              result (reformat/reformat-source
                      input
                      {:line-length 40})]
          (is
           (re-find #"(?m)^\s+\(testing \"inner\"" result)
           "inner testing on its own line")
          (is
           (every? #(<= (count %) 40) (.split result "\n"))
           "no line exceeds limit"))))))

(deftest map-value-pair-splitting-test
  ;; When a map value like (do ...) needs internal breaking, the
  ;; key-value pair should stay on the same line (e.g. :task (do)
  ;; rather than splitting :task onto its own line and (do onto
  ;; the next. This triggers in multi-pair maps where the value
  ;; is long enough that pair-splitting backtracks.
  (testing "reformat-source"
    (testing "given a multi-pair map with do value needing breaks"
      (testing "keeps key and value head on the same line"
        (let [input (str
                     "{lint"
                     " {:doc \"Run linting on src and test\""
                     " :task (do"
                     " (println \"Linting...\")"
                     " (shell \"cmd\"))}}")
              result (reformat/reformat-source
                      input
                      {:line-length 80})]
          (is
           (re-find #"(?m):task \(do$" result)
           ":task and (do stay on the same line"))))))

(deftest let-binding-indent-after-pair-split-test
  ;; When a map pair is split (:task on one line, (let ... on the
  ;; next), the let binding vector's subsequent pairs should be
  ;; indented at the correct column (bracket+1), not at the stale
  ;; column from before the pair split moved the let form.
  (testing "reformat-source"
    (testing "given a map with a let-binding value"
      (testing "indents binding pairs at bracket+1"
        (let [input (str
                     "{:a 1"
                     " :task (let [cmd *cmd-line-args*"
                     " files (get-files dir)]"
                     " (run cmd files))}")
              result (reformat/reformat-source
                      input
                      {:line-length 40})]
          (is
           (re-find
            #"(?m)^ {7}files"
            result)
           (str "files at col 7 (bracket+1),"
                " got:\n"
                result)))))))

(deftest multiline-child-breaking-test
  ;; When a function call has a child that becomes multi-line (e.g. a
  ;; map arg that gets pair-broken), sibling args should be placed on
  ;; their own lines rather than sharing the closing delimiter's line.
  ;; Multi-line pair-grouped children (maps, binding vectors) that move
  ;; to new positions must have stale internal indentation corrected.
  (testing "reformat-source"
    (testing "given a function call with a small map arg"
      (testing "separates sibling args from map"
        (let [input "(f {:a 1 :b 2} \"x\" \"y\")"
              result (reformat/reformat-source
                      input
                      {:line-length 25})]
          (is
           (not (re-find #"\}.*\"" result))
           (str "no string args on same line as },"
                " got:\n"
                result)))))
    (testing "given a map arg that fits on its own line"
      (testing "collapses map to single line after reposition"
        (let [input (str "(p/shell"
                         " {:out :string"
                         " :err :string"
                         " :continue true}"
                         " \"arch\""
                         " \"-x86_64\""
                         " \"/usr/bin/true\")")
              result (reformat/reformat-source
                      input
                      {:line-length 80})]
          (is
           (re-find #"(?m)^ \{:out" result)
           (str "map starts at col 1,"
                " got:\n"
                result))
          (is
           (not (re-find #"(?m)^ {5,}" result))
           (str "no stale indent (5+ spaces),"
                " got:\n"
                result)))))
    (testing "given a map arg too long for one line"
      (testing "re-breaks map with correct indent"
        (let [input (str "(f {:aaa 1 :bbb 2"
                         " :ccc 3} \"x\" \"y\")")
              result (reformat/reformat-source
                      input
                      {:line-length 20})]
          (is
           (not (re-find #"\}.*\"" result))
           (str "no string args on same line as },"
                " got:\n"
                result))
          (is
           (re-find #"(?m)^ \{:aaa 1$" result)
           (str "map at col 1 with first pair,"
                " got:\n"
                result))
          (is
           (re-find #"(?m)^  :bbb 2$" result)
           (str "second pair at col 2,"
                " got:\n"
                result)))))))

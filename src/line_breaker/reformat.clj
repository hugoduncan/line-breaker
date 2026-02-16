(ns line-breaker.reformat
  "Reformat functions for collapsing and re-breaking Clojure code.

  Provides the reformat pipeline: collapse all top-level forms to single
  lines, then iteratively apply forced breaks, pair breaking, fix-source,
  and multiline child breaking until stable."
  (:require
   [line-breaker.fix :as fix]
   [line-breaker.treesitter.node :as node]
   [line-breaker.treesitter.parser :as parser]))

;;; Tree walking

(defn- find-first-preorder
  "Pre-order walk returning the first node for which pred returns true."
  [pred node]
  (when node
    (if (pred node)
      node
      (some #(find-first-preorder pred %) (node/named-children node)))))

;;; Collapse

(defn collapse-top-level-forms
  "Collapse every top-level form to a single line.
  Parses the source, iterates top-level forms in reverse order, and
  recursively replaces inter-child whitespace with single spaces.
  Comment-aware: whole-line comments preserve their leading newline,
  EOL comments preserve their trailing newline (part of the comment node).
  Multi-line string content is not modified.
  Ignore directives are not respected — all forms are collapsed."
  [source]
  (let [tree (parser/parse-source source)
        root (node/root-node tree)
        top-level-forms (node/named-children root)]
    (reduce
     (fn [s form]
       (let [edits (fix/collect-collapse-edits form)]
         (if (seq edits)
           (fix/apply-edits s edits)
           s)))
     source
     (rseq top-level-forms))))

;;; Forced line breaks

(def ^:private default-force-break-rules
  "Rules for inserting forced line breaks in specific forms.
  Each entry maps a head symbol to a rule with :after-indices (0-based
  named-child indices after which to break) and optional :after-types
  (node types after the first occurrence of which to break)."
  {'defn {:after-indices #{1}
          :after-types #{:vec_lit :str_lit}}
   'defn- {:after-indices #{1}
           :after-types #{:vec_lit :str_lit}}
   'defmacro {:after-indices #{1}
              :after-types #{:vec_lit :str_lit}}
   'defmethod {:after-indices #{2}
               :after-types #{:vec_lit}}
   'deftest {:after-indices #{1}}
   'ns {:after-indices #{1}
        :after-types #{:str_lit}}
   'def {:after-indices #{1}
         :after-types #{:str_lit}}
   'defonce {:after-indices #{1}
             :after-types #{:str_lit}}
   'defmulti {:after-indices #{1}
              :after-types #{:str_lit}}
   'fn {:after-types #{:vec_lit}}
   'bound-fn {:after-types #{:vec_lit}}
   'let {:after-types #{:vec_lit}}
   'when-let {:after-types #{:vec_lit}}
   'if-let {:after-types #{:vec_lit}}
   'when-first {:after-types #{:vec_lit}}
   'binding {:after-types #{:vec_lit}}
   'loop {:after-types #{:vec_lit}}
   'doseq {:after-types #{:vec_lit}}
   'for {:after-types #{:vec_lit}}
   'with-open {:after-types #{:vec_lit}}
   'when {:after-indices #{1}}
   'when-not {:after-indices #{1}}
   'if {:after-indices #{1}}
   'if-not {:after-indices #{1}}
   'try {:after-indices #{0}}
   'do {:after-indices #{0}}
   'cond {:after-indices #{0}}
   'condp {:after-indices #{2}}
   'cond-> {:after-indices #{1}}
   'cond->> {:after-indices #{1}}
   'case {:after-indices #{1}}})

(def ^:private multi-arity-parent-syms
  "Symbols whose forms can contain multi-arity clauses."
  #{'defn 'defn- 'defmacro 'fn 'bound-fn})

(def ^:private arity-clause-rule
  "Force-break rule for arity clauses: break after the arg vector."
  {:after-types #{:vec_lit}})

(def ^:private ns-require-import-kws
  "Keywords that identify require/import forms inside ns."
  #{":require" ":import"})

(defn- ns-require-import?
  "Returns true if node is a (:require ...) or (:import ...) inside ns.
  These are list_lit nodes whose first named child is a kwd_lit with text
  :require or :import, and whose parent's head symbol is ns."
  [node]
  (and
   (= :list_lit (node/node-type node))
   (let [first-child (first (node/named-children node))]
     (and
      (= :kwd_lit (node/node-type first-child))
      (contains? ns-require-import-kws (node/node-text first-child))))
   (when-let [parent (node/node-parent node)]
     (= 'ns (fix/get-head-symbol parent)))))

(defn- ns-require-import-rule
  "Build a force-break rule for a require/import form.
  Breaks after the keyword and between every child."
  [node]
  (let [n (count (node/named-children node))]
    {:after-indices (into #{} (range 0 (dec n)))}))

(defn- arity-clause?
  "Returns true if node is an arity clause inside a multi-arity form.
  An arity clause is a list_lit whose first named child is a vec_lit
  and whose parent is a defn-family form."
  [node]
  (and
   (= :list_lit (node/node-type node))
   (let [first-child (first (node/named-children node))]
     (= :vec_lit (node/node-type first-child)))
   (when-let [parent (node/node-parent node)]
     (contains? multi-arity-parent-syms (fix/get-head-symbol parent)))))

(defn- arity-clause-indices
  "Return indices of arity-clause children (list_lit starting with vec_lit)."
  [children]
  (into
   []
   (keep-indexed
    (fn [i child]
      (when (and
             (= :list_lit (node/node-type child))
             (let [fc (first (node/named-children child))]
               (= :vec_lit (node/node-type fc))))
        i)))
   children))

(defn- get-force-break-rule
  "Look up the force-break rule for a list_lit node.
  Checks config's :force-breaks first, then defaults.
  For multi-arity defn-family forms, adds break positions between
  arity clauses. Also matches arity clauses inside multi-arity forms
  and :require/:import forms inside ns."
  [node config]
  (or
   (when-let [head-sym (fix/get-head-symbol node)]
     (let [base-rule (or
                      (get-in config [:force-breaks head-sym])
                      (get default-force-break-rules head-sym))]
       (cond
         (and base-rule (contains? multi-arity-parent-syms head-sym))
         (let [children (node/named-children node)
               arity-idxs (arity-clause-indices children)]
           (if (> (count arity-idxs) 1)
             (update
              base-rule
              :after-indices
              (fn [idxs]
                (into
                 (or idxs #{})
                 (cons (dec (first arity-idxs)) (butlast arity-idxs)))))
             base-rule))
         ;; For do, break between every body form
         (and base-rule (= 'do head-sym))
         (let [n (count (node/named-children node))]
           (if (> n 2)
             (assoc base-rule :after-indices (set (range 0 (dec n))))
             base-rule))
         ;; For ns, break between all clause children (list_lit)
         (and base-rule (= 'ns head-sym))
         (let [children (node/named-children node)
               clause-idxs
               (into
                []
                (keep-indexed
                 (fn [i c]
                   (when (= :list_lit (node/node-type c))
                     i)))
                children)]
           (if (> (count clause-idxs) 1)
             (update
              base-rule
              :after-indices
              (fn [idxs]
                (into (or idxs #{}) (butlast clause-idxs))))
             base-rule))
         :else base-rule)))
   (when (arity-clause? node)
     arity-clause-rule)
   (when (ns-require-import? node)
     (ns-require-import-rule node))))

(defn- forced-break-positions
  "Compute the set of named-child indices after which to insert breaks.
  Merges :after-indices with the index of the first child matching each
  type in :after-types."
  [children rule]
  (let [base (:after-indices rule #{})
        type-indices
        (when-let [types (:after-types rule)]
          (into
           #{}
           (keep
            (fn [type-kw]
              (some
               (fn [i]
                 (when (= type-kw (node/node-type (nth children i)))
                   i))
               (range (count children)))))
           types))]
    (into base type-indices)))

(defn- contiguous-line?
  "Returns true if node1 ends on the same line that node2 starts on.
  Unlike same-line? which compares start lines, this handles multiline
  nodes like docstrings where the end line differs from the start line."
  [node1 node2]
  (let [[_ end-line] (node/node-line-range node1)]
    (= end-line (fix/node-start-line node2))))

(defn- body-separation-positions
  "Indices for body children after the last forced break position that
  share a line with their next sibling.  Skips comment nodes and the
  comment chain from the max break position since the comment-following
  loop already handles those."
  [children break-positions]
  (when (seq break-positions)
    (let [n (count children)
          ;; Skip past comment chain from the max break position
          first-body-idx
          (loop [i (inc (apply max break-positions))]
            (if (and (< i n) (fix/comment-node? (nth children i)))
              (recur (inc i))
              i))]
      (into
       #{}
       (filter
        (fn [i]
          (let [ni (inc i)]
            (and
             (< ni n)
             (not (fix/comment-node? (nth children i)))
             (contiguous-line? (nth children i) (nth children ni))))))
       (range first-body-idx (dec n))))))

(defn- needs-break-or-reindent?
  "Check if a (child, next-child) pair needs a break or re-indent edit.
  When the prev child is a comment, its trailing newline already provides
  line separation, so only the indent column is checked."
  [child next-child indent-col]
  (if (fix/comment-node? child)
    (not= indent-col (fix/form-start-column next-child))
    (or
     (contiguous-line? child next-child)
     (not= indent-col (fix/form-start-column next-child)))))

(defn- form-needs-forced-break?
  "Returns true if any break position has consecutive children on the
  same line, or if any child in the comment chain following a break
  position has wrong indentation."
  [children break-positions indent-col]
  (let [n (count children)]
    (some
     (fn [idx]
       (loop [i idx]
         (let [ni (inc i)]
           (when (< ni n)
             (let [child (nth children i)
                   next-child (nth children ni)]
               (or (needs-break-or-reindent? child next-child indent-col)
                   (when (fix/comment-node? next-child)
                     (recur ni))))))))
     break-positions)))

(defn- generate-forced-break-edits
  "Generate edits to insert forced line breaks in a form.
  Also re-indents children at break positions that are on their own
  line but at the wrong column. Follows comment chains: when a break
  position's next child is a comment, continues generating edits for
  consecutive comments and the first non-comment element after them."
  [node config]
  (let [children (node/named-children node)
        rule (get-force-break-rule node config)
        n (count children)]
    (when rule
      (let [base-positions (forced-break-positions children rule)
            break-positions
            (if (fix/uses-pair-grouping? node config)
              base-positions
              (into
               base-positions
               (body-separation-positions children base-positions)))
            indent-col (fix/indent-column
                        node
                        (fix/get-effective-rule node config))]
        (when (form-needs-forced-break? children break-positions indent-col)
          (into
           []
           (mapcat
            (fn [idx]
              (loop [i idx
                     edits []]
                (let [ni (inc i)]
                  (if (>= ni n)
                    edits
                    (let [child (nth children i)
                          next-child (nth children ni)
                          edit
                          (when (needs-break-or-reindent?
                                 child
                                 next-child
                                 indent-col)
                            (fix/make-break-edit
                             child
                             next-child
                             indent-col))]
                      (if (fix/comment-node? next-child)
                        (recur ni (if edit
                                    (conj edits edit)
                                    edits))
                        (if edit
                          (conj edits edit)
                          edits))))))))
           break-positions))))))

(defn- at-line-start?
  "Returns true if only whitespace precedes node on its line.
  Forms in the middle of a line (after other code) are not at line start.
  Used to skip forced breaks on forms not yet at their final column."
  [node source]
  (let [start-byte (first (node/node-range node))
        start-char (fix/byte-offset->char-index source start-byte)]
    (loop [i (dec start-char)]
      (if (neg? i)
        true
        (let [c (.charAt source i)]
          (cond
            (= c \newline) true
            (Character/isWhitespace c) (recur (dec i))
            :else false))))))

(defn- needs-forced-breaking?
  "Returns true if node is a list_lit matching a force-break rule that
  needs breaks inserted or re-indented.
  When source is provided, only matches forms at the start of their line."
  [node source config]
  (and
   (= :list_lit (node/node-type node))
   (or (nil? source) (at-line-start? node source))
   (let [rule (get-force-break-rule node config)]
     (when rule
       (let [children (node/named-children node)
             positions (forced-break-positions children rule)
             indent-col (fix/indent-column
                         node
                         (fix/get-effective-rule node config))]
         (form-needs-forced-break? children positions indent-col))))))

(defn apply-forced-breaks
  "Insert forced line breaks at structurally significant positions.
  Iteratively finds forms matching force-break rules and inserts line
  breaks, re-parsing between each to maintain correct column positions.
  When check-position? is false, skips the at-line-start? guard for use
  after the pipeline has stabilized and all positions are final."
  ([source config]
   (apply-forced-breaks source config true))
  ([source config check-position?]
   (loop [s source
          iteration 0]
     (if (>= iteration fix/max-iterations)
       s
       (let [tree (parser/parse-source s)
             root (node/root-node tree)
             src-arg (when check-position?
                       s)
             form (find-first-preorder
                   #(needs-forced-breaking? % src-arg config)
                   root)]
         (if-not form
           s
           (let [edits (generate-forced-break-edits form config)]
             (if (and (seq edits) (fix/edits-change-source? s edits))
               (recur (fix/apply-edits s edits) (inc iteration))
               s))))))))

;;; Forced pair breaking

(def ^:private non-binding-pair-rules
  "Pair-grouping rules for non-binding forms (cond, case, condp, cond->).
  These benefit from pair separation before internal line breaking."
  #{:cond :condp :case :cond->})

(def ^:private binding-pair-rules
  "Pair-grouping rules for binding and map forms."
  #{:map :binding-vector})

(defn- pair-group-count
  "Count the number of pairs in a pair-grouped form.
  For maps and binding vectors, all children form pairs. For cond/case/
  condp/cond->, skips the non-pair prefix elements. Comments are
  excluded before counting."
  [node config]
  (let [rule (fix/get-effective-rule node config)
        children (node/named-children node)
        prefix (if (#{:map :binding-vector} rule)
                 0
                 (fix/elements-to-keep-on-first-line rule))
        non-comment (remove fix/comment-node? (drop prefix children))]
    (count (partition-all 2 non-comment))))

(defn- has-unseparated-pairs?
  "Returns true if node has consecutive pairs sharing a line.
  Pairs that should each be on their own line are identified by the
  pair grouping structure. If any pair's first element is on the same
  line as the previous pair's last element, breaking is needed."
  [node config]
  (let [rule (fix/get-effective-rule node config)
        children (node/named-children node)
        prefix (if (#{:map :binding-vector} rule)
                 0
                 (fix/elements-to-keep-on-first-line rule))
        non-comment (remove fix/comment-node? (drop prefix children))
        pairs (partition-all 2 non-comment)]
    (some
     (fn [[prev-pair next-pair]]
       (let [prev-last (last prev-pair)
             next-first (first next-pair)]
         (contiguous-line? prev-last next-first)))
     (partition 2 1 pairs))))

(defn- needs-pair-breaking?
  "Returns true if node is a pair-grouped form with >1 pair that has
  consecutive pairs on the same line. Does not require at-line-start
  because the pre-order walk breaks outermost forms first, ensuring
  inner forms are at their final position when reached.
  When rule-filter is provided, only matches nodes whose effective rule
  is in the filter set."
  ([node config]
   (needs-pair-breaking? node config nil))
  ([node config rule-filter]
   (and
    (fix/breakable-node? node)
    (fix/uses-pair-grouping? node config)
    (or
     (nil? rule-filter)
     (contains? rule-filter (fix/get-effective-rule node config)))
    (> (pair-group-count node config) 1)
    (has-unseparated-pairs? node config))))

(defn- generate-pair-break-edits
  "Generate edits to break a pair-grouped form so each pair is on its
  own line. Also collapses multi-line values so they get properly
  re-broken at their new indent position by subsequent fix-source
  passes. Returns edits or nil."
  [node config]
  (let [rule (fix/get-effective-rule node config)
        children (node/named-children node)
        base-keep-count (fix/elements-to-keep-on-first-line rule)
        indent-col (fix/indent-column node rule)
        breakable-children (drop base-keep-count children)]
    (when (seq breakable-children)
      (let [last-kept (nth children (dec base-keep-count))
            break-edits (fix/generate-paired-edits
                         last-kept
                         breakable-children
                         indent-col)]
        (when (seq break-edits)
          break-edits)))))

(defn apply-pair-breaking
  "Force pair-grouped forms to break so each pair is on its own line.
  Iteratively finds the first qualifying form, applies edits, and
  re-parses until no more forms need breaking.
  When rule-filter is provided, only processes forms whose effective rule
  is in the filter set."
  ([source config]
   (apply-pair-breaking source config nil))
  ([source config rule-filter]
   (loop [s source
          iteration 0]
     (if (>= iteration fix/max-iterations)
       s
       (let [tree (parser/parse-source s)
             root (node/root-node tree)
             form (find-first-preorder
                   #(needs-pair-breaking? % config rule-filter)
                   root)]
         (if-not form
           s
           (let [edits (generate-pair-break-edits form config)]
             (if (seq edits)
               (recur (fix/apply-edits s edits) (inc iteration))
               s))))))))

;;; Multiline child breaking

(defn- has-multiline-child?
  "Returns true if any named child of node spans multiple lines."
  [node]
  (some
   (fn [child]
     (not (fix/single-line-node? child)))
   (node/named-children node)))

(defn- needs-multiline-child-breaking?
  "Returns true if a breakable form has a multi-line child and some
  breakable children still share a line with a sibling."
  [node config]
  (and
   (fix/breakable-node? node)
   (has-multiline-child? node)
   (let [children (node/named-children node)
         rule (fix/get-effective-rule node config)
         keep-n (fix/elements-to-keep-on-first-line rule)
         breakable-children (drop keep-n children)]
     (some
      (fn [[a b]]
        (contiguous-line? a b))
      (partition 2 1 breakable-children)))))

(defn apply-multiline-child-breaking
  "Break forms that contain multi-line children so every child is on
  its own line. Iteratively finds qualifying forms and applies break
  edits, re-parsing between each."
  [source config]
  (loop [s source
         iteration 0]
    (if (>= iteration fix/max-iterations)
      s
      (let [tree (parser/parse-source s)
            root (node/root-node tree)
            form (find-first-preorder
                  #(needs-multiline-child-breaking? % config)
                  root)]
        (if-not form
          s
          (let [edits (fix/break-form form config)]
            (if (and (seq edits) (fix/edits-change-source? s edits))
              (recur (fix/apply-edits s edits) (inc iteration))
              s)))))))

;;; Reformat pipeline

(defn reformat-source
  "Reformat source by collapsing then iteratively applying forced breaks,
  pair breaking, fix-source, and binding pair breaking until stable.
  Non-binding pair-grouped forms (cond, case, condp, cond->) are pair-broken
  before fix-source so pairs are separated before internal line breaking.
  Binding and map forms are pair-broken after fix-source."
  [source config]
  (let [collapsed (collapse-top-level-forms source)]
    (loop [s collapsed
           iteration 0]
      (if (>= iteration fix/max-iterations)
        s
        (let [result (->
                      s
                      (apply-forced-breaks config)
                      (apply-pair-breaking config non-binding-pair-rules)
                      (fix/fix-source config)
                      (apply-pair-breaking config binding-pair-rules)
                      (apply-multiline-child-breaking config))]
          (if (= result s)
            ;; Pipeline stabilized. Apply forced breaks without the
            ;; at-line-start? guard to catch forms like `do` that appear
            ;; mid-line (e.g. as map values). All positions are final now.
            (let [final (->
                         result
                         (apply-forced-breaks config false)
                         (fix/fix-source config)
                         (apply-multiline-child-breaking config))]
              (if (= final result)
                final
                (recur final (inc iteration))))
            (recur result (inc iteration))))))))

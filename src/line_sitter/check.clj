(ns line-sitter.check
  "Line length checking functions."
  (:require
   [clojure.string :as str]
   [line-sitter.treesitter.node :as node]
   [line-sitter.treesitter.parser :as parser]))

(defn check-line-lengths
  "Check a file for lines exceeding max-length.
  Returns vector of violations [{:line n :length len}] where :line is 1-indexed
  and :length is the actual character count of violating lines.
  Empty files return empty vector."
  [file-path max-length]
  (let [content (slurp file-path)
        lines (str/split-lines content)]
    (into []
          (comp
           (map-indexed (fn [idx line]
                          {:line (inc idx) :length (count line)}))
           (filter (fn [{:keys [length]}]
                     (> length max-length))))
          lines)))

(defn format-violation
  "Format a single violation for display.
  Returns string in format: path/to/file.clj:42: line exceeds 80 characters (actual: 95)"
  [{:keys [file line length]} max-length]
  (str file ":" line ": line exceeds " max-length " characters (actual: " length ")"))

(defn report-violations
  "Write formatted violations to stderr.
  Returns the number of violations reported."
  [violations max-length]
  (binding [*out* *err*]
    (doseq [v violations]
      (println (format-violation v max-length))))
  (count violations))

(defn format-summary
  "Format the check summary message.
  Returns nil for single-file checks (no summary needed)."
  [file-count violation-count]
  (when (> file-count 1)
    (if (pos? violation-count)
      (str "Checked " file-count " files, "
           violation-count (if (= 1 violation-count) " violation" " violations") " found")
      (str "Checked " file-count " files, all lines within limit"))))

;;; Ignore directive support

(defn- ignore-marker?
  "Check if node is a dis_expr containing :line-sitter/ignore."
  [node]
  (and (= :dis_expr (node/node-type node))
       (some (fn [child]
               (and (= :kwd_lit (node/node-type child))
                    (= ":line-sitter/ignore" (node/node-text child))))
             (node/named-children node))))

(defn- collect-ignored-ranges
  "Recursively collect ignored line ranges from tree.
  Returns a vector of [start-line end-line] pairs."
  [node]
  (if-not node
    []
    (let [children (node/named-children node)]
      (into []
            (mapcat (fn [child]
                      (if (ignore-marker? child)
                        (when-let [sibling (node/next-named-sibling child)]
                          [(node/node-line-range sibling)])
                        (collect-ignored-ranges child))))
            children))))

(defn find-ignored-ranges
  "Find all line ranges covered by #_:line-sitter/ignore markers.
  Returns a set of [start-line end-line] vectors where lines are 1-indexed
  and both endpoints are inclusive."
  [tree]
  (set (collect-ignored-ranges (node/root-node tree))))

(defn- collect-ignored-byte-ranges
  "Recursively collect ignored byte ranges from tree.
  Returns a vector of [start-byte end-byte] pairs."
  [node]
  (if-not node
    []
    (let [children (node/named-children node)]
      (into []
            (mapcat (fn [child]
                      (if (ignore-marker? child)
                        (when-let [sibling (node/next-named-sibling child)]
                          [(node/node-range sibling)])
                        (collect-ignored-byte-ranges child))))
            children))))

(defn find-ignored-byte-ranges
  "Find all byte ranges covered by #_:line-sitter/ignore markers.
  Returns a set of [start-byte end-byte] vectors where start is inclusive
  and end is exclusive."
  [tree]
  (set (collect-ignored-byte-ranges (node/root-node tree))))

(defn filter-violations
  "Remove violations that fall within ignored line ranges."
  [violations ignored-ranges]
  (if (empty? ignored-ranges)
    violations
    (into []
          (remove (fn [{:keys [line]}]
                    (some (fn [[start end]]
                            (<= start line end))
                          ignored-ranges)))
          violations)))

(defn check-file-with-ignore
  "Check a file for line length violations, respecting ignore directives.
  Returns vector of violations [{:line n :length len}]."
  [file-path max-length]
  (let [violations (check-line-lengths file-path max-length)]
    (if (empty? violations)
      violations
      (let [content (slurp file-path)
            tree (parser/parse-source content)
            ignored-ranges (find-ignored-ranges tree)]
        (filter-violations violations ignored-ranges)))))

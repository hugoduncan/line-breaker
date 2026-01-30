(ns line-sitter.check
  "Line length checking functions."
  (:require
   [clojure.string :as str]))

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

(ns line-breaker.treesitter.parser
  "Clojure interface for parsing source code with tree-sitter.

  Provides resource-safe parser usage and idiomatic Tree access."
  (:require [line-breaker.treesitter.language :as lang])
  (:import [io.github.treesitter.jtreesitter Parser Tree]))

(defmacro with-parser
  "Execute body with a configured Parser bound to sym.

  Opens a Parser, sets the Clojure language, and ensures cleanup via with-open.
  Returns the result of evaluating body."
  [[sym] & body]
  `(with-open [~sym (doto (Parser.)
                      (.setLanguage (lang/load-clojure-language)))]
     ~@body))

(defn parse-source
  "Parse Clojure source code and return a Tree.

  Takes a string of source code and returns a Tree. Throws ex-info if parsing
  fails (e.g., if parsing was halted). Note: tree-sitter still returns a tree
  for source with syntax errors, with ERROR nodes marking invalid sections."
  ^Tree [^String source]
  (with-parser [parser]
    (let [result (.parse parser source)]
      (if (.isPresent result)
        (.get result)
        (throw (ex-info "Parsing failed" {:source source}))))))

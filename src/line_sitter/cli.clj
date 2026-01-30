(ns line-sitter.cli
  "Command-line interface for line-sitter."
  (:require
   [babashka.cli :as cli]))

(def ^:private cli-spec
  "Specification for CLI options."
  {:check {:coerce :boolean
           :desc "Check files for line length violations (default mode)"}
   :fix {:coerce :boolean
         :desc "Fix files by reformatting long lines"}
   :stdout {:coerce :boolean
            :desc "Output reformatted content to stdout"}
   :line-length {:coerce :long
                 :desc "Maximum line length"}
   :help {:coerce :boolean
          :alias :h
          :desc "Show help"}})

(defn parse-args
  "Parse command-line arguments.
  Returns {:opts {...} :args [...]} where :opts contains the parsed options
  and :args contains positional file/directory arguments."
  [args]
  (let [result (cli/parse-args args {:spec cli-spec})
        opts (:opts result)
        positional-args (:args result)]
    {:opts (if (or (:fix opts) (:stdout opts) (:help opts))
             opts
             (assoc opts :check true))
     :args (vec positional-args)}))

(defn -main
  "Entry point for line-sitter CLI."
  [& _args]
  (println "line-sitter")
  (System/exit 0))

(ns line-sitter.cli
  "Command-line interface for line-sitter."
  (:require
   [babashka.cli :as cli]
   [babashka.fs :as fs]
   [clojure.string :as str]))

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
   :quiet {:coerce :boolean
           :alias :q
           :desc "Suppress summary output"}
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

(defn- matches-extension?
  "Check if path has one of the given extensions."
  [path extensions]
  (let [ext-set (set extensions)
        filename (str (fs/file-name path))]
    (some #(str/ends-with? filename %) ext-set)))

(defn- glob-pattern-for-extensions
  "Build a glob pattern matching any of the given extensions.
  Uses ** without trailing slash to match at any depth including root."
  [extensions]
  ;; Remove leading dots for brace expansion
  ;; (glob uses {clj,cljs} not {.clj,.cljs})
  (let [exts (map #(str/replace % #"^\." "") extensions)]
    (str "**.{" (str/join "," exts) "}")))

(defn resolve-files
  "Resolve paths to a sorted vec of absolute file paths.
  Takes a seq of paths (files or directories) and extensions to match.
  For files: includes if extension matches.
  For directories: recursively finds all files matching extensions.
  Throws ex-info with :type :file-error if path does not exist.
  Returns [\".\"] contents when paths is empty."
  [paths extensions]
  (let [paths (if (seq paths) paths ["."])]
    (->> paths
         (mapcat (fn [path]
                   (cond
                     (not (fs/exists? path))
                     (throw (ex-info (str "Path does not exist: " path)
                                     {:type :file-error
                                      :path path}))

                     (fs/directory? path)
                     (let [pattern (glob-pattern-for-extensions extensions)]
                       (fs/glob path pattern))

                     (matches-extension? path extensions)
                     [(fs/absolutize path)]

                     :else
                     [])))
         (map (comp str fs/normalize fs/absolutize))
         sort
         vec)))


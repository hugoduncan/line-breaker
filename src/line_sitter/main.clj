(ns line-sitter.main
  "Main entry point for line-sitter CLI."
  (:require
   [babashka.fs :as fs]
   [line-sitter.cli :as cli]
   [line-sitter.config :as config]))

(def usage-text
  "Usage: line-sitter [options] [files/dirs...]

Reformat Clojure code to enforce maximum line length.

Options:
  --check         Check files for violations (default mode)
  --fix           Fix files by reformatting long lines
  --stdout        Output reformatted content to stdout
  --line-length N Maximum line length (default: 80)
  -h, --help      Show this help

Arguments:
  files/dirs      Files or directories to process (default: current directory)

Examples:
  line-sitter                     Check all files in current directory
  line-sitter src                 Check all files in src directory
  line-sitter --fix src/foo.clj   Fix a specific file
  line-sitter --line-length 100   Check with custom line length

Exit codes:
  0  Success (no violations in check mode, or fix/stdout completed)
  2  Error (invalid arguments, config error, file not found)")

(defn format-error
  "Format an error for stderr output."
  [error-type message]
  (str "line-sitter: " error-type ": " message))

(defn- get-config-dir
  "Determine directory to search for config.
  Uses parent of first file/dir argument, or cwd if none provided."
  [args]
  (if (seq args)
    (let [first-path (first args)]
      (if (fs/directory? first-path)
        first-path
        (or (fs/parent first-path) ".")))
    "."))

(defn- process-stdout
  "Process files in stdout mode. Prints file contents.
  If multiple files, prefixes each with ;;; path header."
  [files]
  (let [multiple? (> (count files) 1)]
    (doseq [file files]
      (when multiple?
        (println (str ";;; " file)))
      (print (slurp file)))))

(defn- process-files
  "Process files according to mode. Currently no-op.
  Returns exit code."
  [files opts]
  (if (:stdout opts)
    (do
      (process-stdout files)
      0)
    ;; --check and --fix are no-ops, exit 0
    0))

(defn run
  "Run line-sitter with given args. Returns exit code.
  Prints to stdout/stderr as appropriate."
  [args]
  (try
    (let [{:keys [opts args]} (cli/parse-args args)]
      (if (:help opts)
        (do
          (println usage-text)
          0)
        (let [config-dir (get-config-dir args)
              config-path (config/find-config-file config-dir)
              base-config (if config-path
                            (config/load-config config-path)
                            config/default-config)
              ;; Merge CLI overrides with config
              final-config (cond-> base-config
                             (:line-length opts)
                             (assoc :line-length (:line-length opts)))
              ;; Validate needed: load-config validates, but when no config file
              ;; exists we use default-config directly with CLI overrides applied.
              _ (config/validate-config final-config)
              files (cli/resolve-files args (:extensions final-config))]
          (process-files files opts))))
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)
            error-type (name (or (:type data) :error))]
        (binding [*out* *err*]
          (println (format-error error-type (ex-message e))))
        2))
    (catch Exception e
      (binding [*out* *err*]
        (println (format-error "error" (ex-message e))))
      2)))

(defn -main
  "Entry point for line-sitter CLI."
  [& args]
  (System/exit (run args)))

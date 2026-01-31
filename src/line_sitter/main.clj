(ns line-sitter.main
  "Main entry point for line-sitter CLI."
  (:require
   [babashka.fs :as fs]
   [line-sitter.check :as check]
   [line-sitter.cli :as cli]
   [line-sitter.config :as config]
   [line-sitter.fix :as fix]))

(def usage-text
  "Usage: line-sitter [options] [files/dirs...]

Reformat Clojure code to enforce maximum line length.

Options:
  --check         Check files for violations (default mode)
  --fix           Fix files by reformatting long lines
  --stdout        Output reformatted content to stdout
  --line-length N Maximum line length (default: 80)
  -q, --quiet     Suppress summary output
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
  1  Violations found (check mode only)
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
  "Process files in stdout mode with fix applied.
  Outputs reformatted content. If multiple files, prefixes each with ;;; path header."
  [files config]
  (let [multiple? (> (count files) 1)]
    (doseq [file files]
      (when multiple?
        (println (str ";;; " file)))
      (let [source (slurp file)
            fixed (fix/fix-source source config)]
        (print fixed)))))

(defn- process-check
  "Process files in check mode.
  Checks each file for line length violations, respecting ignore directives.
  Reports to stderr. Returns exit code: 0 if no violations, 1 if violations."
  [files max-length quiet?]
  (let [all-violations (into []
                             (mapcat (fn [file]
                                       (map #(assoc % :file file)
                                            (check/check-file-with-ignore
                                             file max-length))))
                             files)
        violation-count (check/report-violations all-violations max-length)]
    (when-not quiet?
      (when-let [summary (check/format-summary (count files) violation-count)]
        (binding [*out* *err*]
          (println summary))))
    (if (seq all-violations) 1 0)))

(defn- process-fix
  "Process files in fix mode.
  Reads each file, applies fix algorithm, writes back in place.
  Reports fixed files to stderr unless quiet. Returns 0 on success."
  [files config quiet?]
  (doseq [file files]
    (let [source (slurp file)
          fixed (fix/fix-source source config)]
      (when (not= source fixed)
        (spit file fixed)
        (when-not quiet?
          (binding [*out* *err*]
            (println (str "Fixed: " file)))))))
  0)

(defn- process-files
  "Process files according to mode.
  Returns exit code."
  [files opts config]
  (cond
    (:stdout opts)
    (do
      (process-stdout files config)
      0)

    (:fix opts)
    (process-fix files config (:quiet opts))

    (:check opts)
    (process-check files (:line-length config) (:quiet opts))

    :else
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
          (process-files files opts final-config))))
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

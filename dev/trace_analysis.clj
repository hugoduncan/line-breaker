(ns trace-analysis
  "Run tracing analysis against the project's own codebase and tests.

  Evaluate forms in this namespace at the REPL to collect and
  analyze trace data."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [line-breaker.config :as config]
   [line-breaker.reformat :as reformat]
   [line-breaker.trace :as trace]))

;;; Helpers

(defn clj-files
  "Return all .clj files under `dir`."
  [dir]
  (->> (file-seq (io/file dir))
       (filter #(str/ends-with? (.getName %) ".clj"))
       (sort-by str)
       vec))

(defn trace-reformat
  "Reformat `source` with tracing, return
  {:result <string> :events <vector of trace maps>}."
  [source config]
  (let [events (atom [])]
    (binding [trace/*trace-fn* (trace/collecting-trace-fn events)]
      (let [result (reformat/reformat-source source config)]
        {:result result
         :events @events}))))

(defn trace-file
  "Trace reformat of a single file. Returns trace data map."
  [file config]
  (let [source (slurp file)
        {:keys [events]} (trace-reformat source config)]
    {:file (str file)
     :events events}))

(defn trace-directory
  "Trace reformat of all .clj files under `dir`."
  [dir config]
  (mapv #(trace-file % config) (clj-files dir)))

;;; Analysis

(defn summarize-pipeline-events
  "Summarize pipeline-level events across all files."
  [all-traces]
  (let [pipeline-events
        (->> all-traces
             (mapcat :events)
             (filter #(= :pipeline (:level %))))]
    {:total-pipeline-iterations (count pipeline-events)
     :max-iteration
     (if (seq pipeline-events)
       (apply max (mapv :iteration pipeline-events))
       0)
     :changed-steps-freq
     (->> pipeline-events
          (mapcat :changed-steps)
          frequencies
          (sort-by val >)
          vec)
     :files-with-multiple-iterations
     (->> all-traces
          (keep
           (fn [{:keys [file events]}]
             (let [pipe-evts
                   (filterv
                    #(= :pipeline (:level %))
                    events)]
               (when (> (count pipe-evts) 1)
                 {:file file
                  :iterations (count pipe-evts)}))))
          vec)}))

(defn summarize-sub-loop-events
  "Summarize events for a given sub-loop level."
  [all-traces level]
  (let [events (->> all-traces
                    (mapcat :events)
                    (filter #(= level (:level %))))]
    {:level level
     :total-events (count events)
     :stable-events
     (count (filter #(= :stable (:outcome %)) events))
     :max-iterations-to-stable
     (let [stables (keep :iterations
                         (filter #(= :stable (:outcome %))
                                 events))]
       (when (seq stables)
         (apply max stables)))
     :form-iteration-events
     (count (filter :form events))
     :form-types-freq
     (->> events
          (keep #(get-in % [:form :type]))
          frequencies
          (sort-by val >)
          vec)}))

(defn summarize-stale-indent-events
  "Summarize stale-indent events."
  [all-traces]
  (let [events (->> all-traces
                    (mapcat :events)
                    (filter #(= :stale-indent (:level %))))]
    {:total-stale-indent-triggers (count events)
     :forms
     (->> events
          (mapv #(get-in % [:form :snippet])))}))

(defn unchecked-forced-breaks-summary
  "Check whether unchecked forced breaks ever trigger changes."
  [all-traces]
  (let [pipeline-events
        (->> all-traces
             (mapcat :events)
             (filter #(= :pipeline (:level %))))]
    {:unchecked-forced-break-triggers
     (count
      (filter
       #(some #{:forced-breaks-unchecked} (:changed-steps %))
       pipeline-events))
     :total-pipeline-iterations
     (count pipeline-events)}))

(defn full-analysis
  "Run full analysis and return summary map."
  [all-traces]
  {:pipeline (summarize-pipeline-events all-traces)
   :forced-breaks
   (summarize-sub-loop-events all-traces :forced-breaks)
   :pair-breaking
   (summarize-sub-loop-events all-traces :pair-breaking)
   :fix-source
   (summarize-sub-loop-events all-traces :fix-source)
   :multiline-child
   (summarize-sub-loop-events all-traces :multiline-child)
   :stale-indent
   (summarize-stale-indent-events all-traces)
   :unchecked-forced-breaks
   (unchecked-forced-breaks-summary all-traces)})

(defn format-analysis
  "Format analysis map as markdown string."
  [analysis]
  (let [sb (StringBuilder.)]
    (.append sb "# Iteration Analysis\n\n")
    (.append sb
             (str "Analysis of the reformat pipeline's "
                  "iteration/convergence behavior.\n\n"))

    ;; Pipeline
    (.append sb "## Pipeline (outer loop)\n\n")
    (let [p (:pipeline analysis)]
      (.append sb
               (str "- Total pipeline iterations: "
                    (:total-pipeline-iterations p) "\n"))
      (.append sb
               (str "- Max iteration count: "
                    (:max-iteration p) "\n"))
      (.append sb "\nChanged steps frequency:\n\n")
      (doseq [[step cnt] (:changed-steps-freq p)]
        (.append sb (str "| `" step "` | " cnt " |\n")))
      (.append sb
               "\nFiles requiring multiple iterations:\n\n")
      (doseq [{:keys [file iterations]}
              (:files-with-multiple-iterations p)]
        (.append sb
                 (str "- " file ": " iterations
                      " iterations\n"))))

    ;; Sub-loops
    (doseq [level [:forced-breaks :pair-breaking
                   :fix-source :multiline-child]]
      (let [s (get analysis level)]
        (.append sb
                 (str "\n## " (name level) " sub-loop\n\n"))
        (.append sb
                 (str "- Total events: "
                      (:total-events s) "\n"))
        (.append sb
                 (str "- Stable (0 iterations): "
                      (:stable-events s) "\n"))
        (.append sb
                 (str "- Max iterations to stable: "
                      (:max-iterations-to-stable s) "\n"))
        (.append sb
                 (str "- Form processing events: "
                      (:form-iteration-events s) "\n"))
        (when (seq (:form-types-freq s))
          (.append sb "\nForm types frequency:\n\n")
          (doseq [[tp cnt] (:form-types-freq s)]
            (.append sb
                     (str "| `" tp "` | " cnt " |\n"))))))

    ;; Stale indent
    (.append sb "\n## Stale indent detection\n\n")
    (let [si (:stale-indent analysis)]
      (.append sb
               (str "- Total triggers: "
                    (:total-stale-indent-triggers si) "\n"))
      (when (seq (:forms si))
        (.append sb "\nForms:\n\n")
        (doseq [f (:forms si)]
          (.append sb (str "- `" f "`\n")))))

    ;; Unchecked forced breaks
    (.append sb
             "\n## Unchecked forced breaks (2nd pass)\n\n")
    (let [u (:unchecked-forced-breaks analysis)]
      (.append sb
               (str "- Triggers: "
                    (:unchecked-forced-break-triggers u)
                    " / "
                    (:total-pipeline-iterations u)
                    " pipeline iterations\n")))

    (.toString sb)))

;;; Entry point

(defn run-analysis!
  "Run full analysis on src/ and write to doc/."
  []
  (let [config (config/default-config)
        traces (trace-directory "src" config)
        analysis (full-analysis traces)
        md (format-analysis analysis)]
    (spit "doc/iteration-analysis.md" md)
    (println "Wrote doc/iteration-analysis.md")
    analysis))

(comment
  (run-analysis!))

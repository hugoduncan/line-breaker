(ns line-breaker.trace
  "Tracing for iteration/convergence analysis.

  Bind `*trace-fn*` to a function of one argument (a trace event map)
  to capture events from the reformat pipeline.  When nil (the default),
  tracing is a no-op."
  (:require
   [line-breaker.treesitter.node :as node]))

(def ^:dynamic *trace-fn*
  "When non-nil, called with a trace-event map for every traced point."
  nil)

(defn trace!
  "Emit a trace event when tracing is active."
  [event]
  (when *trace-fn*
    (*trace-fn* event)))

(defn node-summary
  "Short description of a node for trace output."
  [node]
  (when node
    (let [text (node/node-text node)
          pos (node/node-position node)]
      {:type (node/node-type node)
       :row (:row pos)
       :col (:column pos)
       :snippet (subs text 0 (min (count text) 60))})))

(defn collecting-trace-fn
  "Return a trace-fn that conj's events onto an atom."
  [events-atom]
  (fn [event]
    (swap! events-atom conj event)))

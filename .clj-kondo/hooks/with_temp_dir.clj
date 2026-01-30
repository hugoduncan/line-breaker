(ns hooks.with-temp-dir
  (:require [clj-kondo.hooks-api :as api]))

(defn with-temp-dir
  "Hook for with-temp-dir macro that binds a single symbol."
  [{:keys [node]}]
  (let [[binding-vec & body] (rest (:children node))
        [sym] (:children binding-vec)]
    {:node (api/list-node
            (list*
             (api/token-node 'let)
             (api/vector-node [sym (api/token-node nil)])
             body))}))

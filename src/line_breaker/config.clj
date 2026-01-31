(ns line-sitter.config
  "Configuration loading, merging, and validation for line-sitter."
  (:require
   [babashka.fs :as fs]
   [clojure.edn :as edn]))

(def default-config
  "Default configuration values."
  {:line-length 80
   :extensions [".clj" ".cljs" ".cljc" ".edn"]
   :indents {}})

(def config-filename ".line-sitter.edn")

(defn find-config-file
  "Walk up from `dir` looking for `.line-sitter.edn`.
  Returns the path to the config file if found, nil otherwise."
  [dir]
  (let [start-dir (fs/normalize (fs/absolutize dir))]
    (loop [current start-dir]
      (let [config-path (fs/path current config-filename)
            parent (fs/parent current)]
        (cond
          (fs/exists? config-path) (str config-path)
          (nil? parent) nil
          :else (recur parent))))))

(defn deep-merge
  "Deep merge maps. Values from later maps override earlier ones.
  Non-map values are replaced, not merged."
  [& maps]
  (reduce
   (fn [acc m]
     (reduce-kv
      (fn [a k v]
        (if (and (map? v) (map? (get a k)))
          (assoc a k (deep-merge (get a k) v))
          (assoc a k v)))
      acc
      m))
   {}
   maps))

(defn validate-config
  "Validate configuration map. Throws ex-info with :type :config-error
  if validation fails. Returns config if valid."
  [config]
  (let [{:keys [line-length extensions indents]} config]
    (when-not (pos-int? line-length)
      (throw (ex-info "Invalid config: :line-length must be a positive integer"
                      {:type :config-error
                       :key :line-length
                       :value line-length})))
    (when-not (and (vector? extensions)
                   (every? string? extensions))
      (throw (ex-info "Invalid config: :extensions must be a vector of strings"
                      {:type :config-error
                       :key :extensions
                       :value extensions})))
    (when-not (map? indents)
      (throw (ex-info "Invalid config: :indents must be a map"
                      {:type :config-error
                       :key :indents
                       :value indents})))
    config))

(defn load-config
  "Load configuration from file path, merging with defaults.
  Returns merged and validated config.
  Throws ex-info on read or validation error."
  [config-path]
  (let [file-config (try
                      (edn/read-string (slurp config-path))
                      (catch Exception e
                        (throw
                         (ex-info
                          (str "Failed to read config: " (.getMessage e))
                          {:type :config-error :path config-path}
                          e))))]
    (when-not (map? file-config)
      (throw (ex-info "Invalid config: file must contain a map"
                      {:type :config-error
                       :path config-path})))
    (-> (deep-merge default-config file-config)
        validate-config)))

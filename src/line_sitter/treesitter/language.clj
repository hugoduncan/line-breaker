(ns line-sitter.treesitter.language
  "Load the tree-sitter Clojure grammar from native libraries.

  Discovery order:
  1. LINE_SITTER_NATIVE_LIB environment variable (explicit path)
  2. native/<os>-<arch>/ on classpath resources
  3. java.library.path"
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.lang.foreign Arena SymbolLookup]
           [java.nio.file Path]
           [io.github.treesitter.jtreesitter Language]))

(defn- detect-os
  "Detect operating system: darwin or linux."
  []
  (let [os-name (System/getProperty "os.name")]
    (cond
      (re-find #"(?i)mac" os-name) "darwin"
      (re-find #"(?i)linux" os-name) "linux"
      :else (throw (ex-info (str "Unsupported OS: " os-name)
                            {:os os-name})))))

(defn- detect-arch
  "Detect architecture: aarch64 or x86_64."
  []
  (let [arch (System/getProperty "os.arch")]
    (case arch
      ("aarch64" "arm64") "aarch64"
      ("amd64" "x86_64") "x86_64"
      (throw (ex-info (str "Unsupported architecture: " arch)
                      {:arch arch})))))

(defn- library-name
  "Get platform-specific library filename."
  [os]
  (case os
    "darwin" "libtree-sitter-clojure.dylib"
    "linux" "libtree-sitter-clojure.so"))

(defn- extract-resource-to-temp
  "Extract a classpath resource to a temporary file.
  Returns the Path to the temp file, or nil if resource not found."
  [resource-path]
  (when-let [url (io/resource resource-path)]
    (let [temp-dir (fs/create-temp-dir {:prefix "line-sitter-"})
          temp-file (fs/path temp-dir (fs/file-name resource-path))]
      (with-open [in (io/input-stream url)]
        (io/copy in (fs/file temp-file)))
      temp-file)))

(defn- find-in-library-path
  "Search for library in java.library.path directories.
  Returns the Path if found, nil otherwise."
  [lib-name]
  (let [lib-path (System/getProperty "java.library.path")]
    (when lib-path
      (some (fn [dir]
              (let [candidate (fs/path dir lib-name)]
                (when (fs/exists? candidate)
                  candidate)))
            (str/split lib-path
                       (re-pattern (System/getProperty "path.separator")))))))

(defn- find-library-path
  "Find the native library path using discovery order.
  Returns [Path source] where source is :env-var, :classpath, or :library-path.
  Throws ex-info if not found."
  []
  (let [os (detect-os)
        arch (detect-arch)
        lib-name (library-name os)
        env-path (System/getenv "LINE_SITTER_NATIVE_LIB")
        resource-path (str "native/" os "-" arch "/" lib-name)]
    (cond
      ;; 1. Explicit path via environment variable
      (and env-path (fs/exists? env-path))
      [(fs/path env-path) :env-var]

      ;; 2. Classpath resource (extract to temp)
      :else
      (if-let [extracted (extract-resource-to-temp resource-path)]
        [extracted :classpath]
        ;; 3. java.library.path
        (if-let [lib-path-result (find-in-library-path lib-name)]
          [lib-path-result :library-path]
          ;; Not found
          (throw (ex-info (str "Could not find native library: " lib-name)
                          {:library lib-name
                           :os os
                           :arch arch
                           :env-var-checked (boolean env-path)
                           :resource-path resource-path
                           :library-path
                           (System/getProperty "java.library.path")})))))))

(defn load-clojure-language
  "Load the Clojure language grammar from a native library.

  Searches for the library in order:
  1. LINE_SITTER_NATIVE_LIB environment variable
  2. native/<os>-<arch>/ on classpath
  3. java.library.path

  Returns a jtreesitter Language instance.
  Throws ex-info if the library cannot be found or loaded."
  []
  (let [[lib-path _source] (find-library-path)
        arena (Arena/global)
        symbols (SymbolLookup/libraryLookup ^Path lib-path arena)]
    (Language/load symbols "tree_sitter_clojure")))

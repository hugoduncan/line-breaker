(ns line-sitter.treesitter.language
  "Load the tree-sitter Clojure grammar from native libraries.

  Discovery order:
  1. LINE_SITTER_NATIVE_LIB environment variable (explicit path)
  2. native/<os>-<arch>/ on classpath resources
  3. java.library.path"
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [line-sitter.platform :as platform])
  (:import [java.lang.foreign Arena SymbolLookup]
           [java.nio.file Path]
           [io.github.treesitter.jtreesitter Language]))

(defn- library-name
  "Get platform-specific library filename."
  [os]
  (str "libtree-sitter-clojure" (platform/library-extension os)))

(defn- extract-resource-to-temp
  "Extract a classpath resource to a temporary file.
  Returns the Path to the temp file, or nil if resource not found.
  Files are marked for deletion when the JVM exits."
  [resource-path]
  (when-let [url (io/resource resource-path)]
    (let [temp-dir (fs/create-temp-dir {:prefix "line-sitter-"})
          temp-file (fs/path temp-dir (fs/file-name resource-path))]
      (with-open [in (io/input-stream url)]
        (io/copy in (fs/file temp-file)))
      ;; Mark for deletion on JVM exit.
      ;; Delete file first, then directory (deleteOnExit order is LIFO).
      (.deleteOnExit (fs/file temp-dir))
      (.deleteOnExit (fs/file temp-file))
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

(defn- get-env-lib-path
  "Get the native library path from environment variable.
  Returns the path string or nil if not set."
  []
  (System/getenv "LINE_SITTER_NATIVE_LIB"))

(defn- find-library-path
  "Find the native library path using discovery order.
  Returns [Path source] where source is :env-var, :classpath, or :library-path.
  Throws ex-info if not found."
  []
  (let [os (platform/detect-os)
        arch (platform/detect-arch)
        lib-name (library-name os)
        env-path (get-env-lib-path)
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

(defn- load-clojure-language*
  "Internal: load the Clojure language grammar from a native library."
  []
  (let [[lib-path _source] (find-library-path)
        arena (Arena/global)
        symbols (SymbolLookup/libraryLookup ^Path lib-path arena)]
    (Language/load symbols "tree_sitter_clojure")))

(def ^:private clojure-language-delay
  "Delay for one-time language loading."
  (delay (load-clojure-language*)))

(defn load-clojure-language
  "Load the Clojure language grammar from a native library.

  Searches for the library in order:
  1. LINE_SITTER_NATIVE_LIB environment variable
  2. native/<os>-<arch>/ on classpath
  3. java.library.path

  Returns a jtreesitter Language instance (memoized after first load).
  Throws ex-info if the library cannot be found or loaded."
  []
  @clojure-language-delay)

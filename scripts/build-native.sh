#!/usr/bin/env bb

;; Build tree-sitter-clojure native library for the current platform.
;; Outputs to resources/native/<os>-<arch>/ for bundling in the JAR.
;;
;; Usage: bb build-native
;;
;; Requirements:
;; - C compiler (cc)
;; - git
;;
;; For CI: Run this script on each target platform (macOS, Linux).
;; Cross-compilation is possible but not implemented here.

(require '[babashka.fs :as fs]
         '[babashka.process :refer [shell]])

(def project-root (fs/parent (fs/parent *file*)))
(def build-dir (fs/path project-root ".build" "tree-sitter-clojure"))
(def resources-dir (fs/path project-root "resources"))

(defn detect-os
  "Detect operating system: darwin or linux."
  []
  (let [os-name (System/getProperty "os.name")]
    (cond
      (re-find #"(?i)mac" os-name) "darwin"
      (re-find #"(?i)linux" os-name) "linux"
      :else (throw (ex-info (str "Unsupported OS: " os-name) {:os os-name})))))

(defn detect-arch
  "Detect architecture: aarch64 or x86_64."
  []
  (let [arch (System/getProperty "os.arch")]
    (case arch
      "aarch64" "aarch64"
      "arm64" "aarch64"
      "amd64" "x86_64"
      "x86_64" "x86_64"
      (throw (ex-info (str "Unsupported architecture: " arch) {:arch arch})))))

(defn library-name
  "Get platform-specific library name."
  [os]
  (case os
    "darwin" "libtree-sitter-clojure.dylib"
    "linux" "libtree-sitter-clojure.so"))

(defn clone-grammar
  "Clone tree-sitter-clojure repository if not present."
  []
  (when-not (fs/exists? build-dir)
    (println "Cloning tree-sitter-clojure...")
    (fs/create-dirs (fs/parent build-dir))
    (shell {:dir (fs/parent build-dir)}
           "git" "clone" "--depth" "1"
           "https://github.com/sogaiu/tree-sitter-clojure"
           (str (fs/file-name build-dir)))))

(defn compile-library
  "Compile the native library for the current platform."
  [os arch]
  (let [lib-name (library-name os)
        output-dir (fs/path resources-dir "native" (str os "-" arch))
        output-path (fs/path output-dir lib-name)]
    (fs/create-dirs output-dir)
    (println (str "Compiling " lib-name " for " os "-" arch "..."))
    (shell {:dir (str build-dir)}
           "cc" "-shared" "-fPIC"
           "-I" "src"
           "src/parser.c"
           "-o" (str output-path))
    (println (str "Built: " output-path))
    output-path))

(defn -main
  []
  (let [os (detect-os)
        arch (detect-arch)]
    (println (str "Building for " os "-" arch))
    (clone-grammar)
    (compile-library os arch)
    (println "Done.")))

(-main)

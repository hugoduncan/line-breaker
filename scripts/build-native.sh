#!/usr/bin/env bb

;; Build tree-sitter native libraries for the current platform.
;; Outputs to resources/native/<os>-<arch>/ for bundling in the JAR.
;;
;; Builds:
;; - libtree-sitter (core library required by jtreesitter)
;; - libtree-sitter-clojure (Clojure grammar)
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
(def build-base-dir (fs/path project-root ".build"))
(def clojure-build-dir (fs/path build-base-dir "tree-sitter-clojure"))
(def core-build-dir (fs/path build-base-dir "tree-sitter"))
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
  [os base-name]
  (case os
    "darwin" (str "lib" base-name ".dylib")
    "linux" (str "lib" base-name ".so")))

(defn clone-repo
  "Clone a repository if not present."
  [url build-dir]
  (when-not (fs/exists? build-dir)
    (println (str "Cloning " (fs/file-name build-dir) "..."))
    (fs/create-dirs (fs/parent build-dir))
    (shell {:dir (fs/parent build-dir)}
           "git" "clone" "--depth" "1" url
           (str (fs/file-name build-dir)))))

(defn compile-clojure-grammar
  "Compile tree-sitter-clojure grammar for the current platform."
  [os arch output-dir]
  (let [lib-name (library-name os "tree-sitter-clojure")
        output-path (fs/path output-dir lib-name)]
    (println (str "Compiling " lib-name " for " os "-" arch "..."))
    (shell {:dir (str clojure-build-dir)}
           "cc" "-shared" "-fPIC"
           "-I" "src"
           "src/parser.c"
           "-o" (str output-path))
    (println (str "Built: " output-path))
    output-path))

(defn compile-core-library
  "Compile tree-sitter core library for the current platform."
  [os arch output-dir]
  (let [lib-name (library-name os "tree-sitter")
        output-path (fs/path output-dir lib-name)
        lib-dir (fs/path core-build-dir "lib")]
    (println (str "Compiling " lib-name " for " os "-" arch "..."))
    ;; tree-sitter core has its source in lib/src/
    (shell {:dir (str core-build-dir)}
           "cc" "-shared" "-fPIC"
           "-I" (str lib-dir "/include")
           "-I" (str lib-dir "/src")
           (str lib-dir "/src/lib.c")
           "-o" (str output-path))
    (println (str "Built: " output-path))
    output-path))

(defn -main
  []
  (let [os (detect-os)
        arch (detect-arch)
        output-dir (fs/path resources-dir "native" (str os "-" arch))]
    (println (str "Building for " os "-" arch))
    (fs/create-dirs output-dir)
    ;; Clone repositories
    (clone-repo "https://github.com/tree-sitter/tree-sitter" core-build-dir)
    (clone-repo "https://github.com/sogaiu/tree-sitter-clojure"
                clojure-build-dir)
    ;; Compile libraries
    (compile-core-library os arch output-dir)
    (compile-clojure-grammar os arch output-dir)
    (println "Done.")))

(-main)

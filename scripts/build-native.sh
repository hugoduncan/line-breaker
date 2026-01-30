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

(defn shell-with-context
  "Execute shell command with context for error messages.
  context-msg describes what step is being performed."
  [context-msg opts & args]
  (try
    (apply shell opts args)
    (catch Exception e
      (throw (ex-info (str "Build failed: " context-msg)
                      {:step context-msg
                       :opts opts
                       :args args}
                      e)))))

;; Add src to classpath for platform namespace
(babashka.classpath/add-classpath (str (fs/path (fs/parent (fs/parent *file*)) "src")))

(require '[line-sitter.platform :as platform])

(def project-root (fs/parent (fs/parent *file*)))
(def build-base-dir (fs/path project-root ".build"))
(def clojure-build-dir (fs/path build-base-dir "tree-sitter-clojure"))
(def core-build-dir (fs/path build-base-dir "tree-sitter"))
(def resources-dir (fs/path project-root "resources"))

(defn library-name
  "Get platform-specific library name."
  [os base-name]
  (str "lib" base-name (platform/library-extension os)))

(defn clone-repo
  "Clone a repository if not present."
  [url build-dir]
  (when-not (fs/exists? build-dir)
    (println (str "Cloning " (fs/file-name build-dir) "..."))
    (fs/create-dirs (fs/parent build-dir))
    (shell-with-context (str "cloning " (fs/file-name build-dir))
                        {:dir (fs/parent build-dir)}
                        "git" "clone" "--depth" "1" url
                        (str (fs/file-name build-dir)))))

(defn compile-clojure-grammar
  "Compile tree-sitter-clojure grammar for the current platform."
  [os arch output-dir]
  (let [lib-name (library-name os "tree-sitter-clojure")
        output-path (fs/path output-dir lib-name)]
    (println (str "Compiling " lib-name " for " os "-" arch "..."))
    (shell-with-context "compiling Clojure grammar"
                        {:dir (str clojure-build-dir)}
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
    (shell-with-context "compiling tree-sitter core library"
                        {:dir (str core-build-dir)}
                        "cc" "-shared" "-fPIC"
                        "-I" (str lib-dir "/include")
                        "-I" (str lib-dir "/src")
                        (str lib-dir "/src/lib.c")
                        "-o" (str output-path))
    (println (str "Built: " output-path))
    output-path))

(defn -main
  []
  (let [os (platform/detect-os)
        arch (platform/detect-arch)
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

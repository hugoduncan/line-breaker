(ns build
  "Build configuration for line-sitter."
  (:require
   [clojure.tools.build.api :as b]))

(def lib 'io.github.hugoduncan/line-sitter)
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def java-class-dir "classes")
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

(defn clean
  "Remove build artifacts."
  [_]
  (b/delete {:path "target"})
  (b/delete {:path java-class-dir}))

(defn javac
  "Compile Java sources (NativeLoader for jtreesitter)."
  [_]
  (b/javac {:src-dirs ["java"]
            :class-dir java-class-dir
            :basis (b/create-basis {:project "deps.edn"
                                    :aliases [:native]})
            :javac-opts ["--release" "23"]}))

(defn uber
  "Build an uberjar."
  [_]
  (clean nil)
  (javac nil)
  ;; Copy resources and compiled Java classes for jar inclusion
  (b/copy-dir {:src-dirs ["resources" "classes"]
               :target-dir class-dir})
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
  (b/compile-clj {:basis (b/create-basis {:project "deps.edn"
                                          :aliases [:native]})
                  :ns-compile '[line-sitter.main]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis (b/create-basis {:project "deps.edn"
                                   :aliases [:native]})
           :main 'line-sitter.main}))

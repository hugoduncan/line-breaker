(ns build
  "Build configuration for line-breaker."
  (:require
   [babashka.process :as p]
   [clojure.tools.build.api :as b]))

(def lib 'io.github.hugoduncan/line-breaker)
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
                  :ns-compile '[line-breaker.main]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis (b/create-basis {:project "deps.edn"
                                   :aliases [:native]})
           :main 'line-breaker.main}))

(defn- find-native-image
  "Find native-image executable in GRAALVM_HOME, JAVA_HOME, or PATH."
  []
  (let [graalvm-home (System/getenv "GRAALVM_HOME")
        java-home (System/getenv "JAVA_HOME")]
    (cond
      graalvm-home (str graalvm-home "/bin/native-image")
      java-home (str java-home "/bin/native-image")
      :else "native-image")))

(defn native-image
  "Build native executable using GraalVM native-image.
  Requires GraalVM 25+ with native-image installed.
  Set GRAALVM_HOME or JAVA_HOME to the GraalVM installation directory.
  Throws an exception if the build fails."
  [_]
  (uber nil)
  (println "Building native image...")
  (let [native-image-cmd (find-native-image)
        result (p/shell {:out :inherit :err :inherit :continue true}
                        native-image-cmd "-jar" uber-file
                        "-o" "target/line-breaker")
        exit-code (:exit result)]
    (if (zero? exit-code)
      (println "Native image built: target/line-breaker")
      (throw (ex-info "native-image build failed"
                      {:exit-code exit-code})))))

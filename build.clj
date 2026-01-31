(ns build
  "Build configuration for line-breaker."
  (:require
   [babashka.fs :as fs]
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
  ([]
   (find-native-image (System/getenv "GRAALVM_HOME")))
  ([graalvm-home]
   (let [java-home (System/getenv "JAVA_HOME")]
     (cond
       graalvm-home (str graalvm-home "/bin/native-image")
       java-home (str java-home "/bin/native-image")
       :else "native-image"))))

(defn- verify-rosetta-available
  "Check if Rosetta 2 is available on the system."
  []
  (try
    (let [result (p/shell {:out :string :err :string :continue true}
                          "arch" "-x86_64" "/usr/bin/true")]
      (zero? (:exit result)))
    (catch Exception _
      false)))

(defn- build-single-arch
  "Build native image for a single architecture.
  arch is :arm64 or :x86_64. For x86_64 on ARM Mac, uses Rosetta.
  Returns the path to the built binary."
  [graalvm-home uber-file output-path arch]
  (let [native-image-cmd (find-native-image graalvm-home)
        base-args [native-image-cmd "-jar" uber-file "-o" output-path]
        all-args (if (= arch :x86_64)
                   (into ["arch" "-x86_64"] base-args)
                   base-args)]
    (println (format "  Building %s..." (name arch)))
    (apply p/shell {:out :inherit :err :inherit} all-args)
    output-path))

(defn native-image
  "Build native executable using GraalVM native-image.
  Requires GraalVM 25+ with native-image installed.
  Set GRAALVM_HOME or JAVA_HOME to the GraalVM installation directory.

  Options:
  - :universal true - Build macOS universal binary (arm64 + x86_64).
    Requires GRAALVM_HOME (arm64) and GRAALVM_HOME_X86_64 environment variables.
    Only supported on macOS with Rosetta 2 installed.

  Throws an exception if the build fails."
  [{:keys [universal]}]
  (uber nil)
  (if universal
    ;; Universal binary build for macOS
    (let [graalvm-home (System/getenv "GRAALVM_HOME")
          graalvm-home-x86 (System/getenv "GRAALVM_HOME_X86_64")
          arm64-binary "target/line-breaker-arm64-temp"
          x86-binary "target/line-breaker-x86_64-temp"
          output-binary "target/line-breaker"]
      (when-not graalvm-home
        (throw (ex-info "GRAALVM_HOME not set" {})))
      (when-not graalvm-home-x86
        (throw
         (ex-info
          "GRAALVM_HOME_X86_64 not set (required for universal builds)"
          {})))
      (when-not (verify-rosetta-available)
        (throw
         (ex-info
          (str "Rosetta 2 required for universal builds. "
               "Install with: softwareupdate --install-rosetta")
          {})))
      (println "Building universal native image (arm64 + x86_64)...")
      ;; Build arm64
      (build-single-arch graalvm-home uber-file arm64-binary :arm64)
      ;; Build x86_64 under Rosetta
      (build-single-arch graalvm-home-x86 uber-file x86-binary :x86_64)
      ;; Combine with lipo
      (println "  Combining architectures with lipo...")
      (p/shell {:out :inherit :err :inherit}
               "lipo" "-create" arm64-binary x86-binary "-output" output-binary)
      ;; Verify
      (p/shell {:out :inherit :err :inherit} "lipo" "-info" output-binary)
      ;; Cleanup
      (fs/delete-if-exists arm64-binary)
      (fs/delete-if-exists x86-binary)
      (println "Universal native image built: target/line-breaker"))
    ;; Standard single-architecture build
    (do
      (println "Building native image...")
      (let [native-image-cmd (find-native-image)
            result (p/shell {:out :inherit :err :inherit :continue true}
                            native-image-cmd "-jar" uber-file
                            "-o" "target/line-breaker")
            exit-code (:exit result)]
        (if (zero? exit-code)
          (println "Native image built: target/line-breaker")
          (throw (ex-info "native-image build failed"
                          {:exit-code exit-code})))))))

(ns line-breaker.platform
  "Platform detection utilities for native library loading.")

(defn detect-os
  "Detect operating system.
  Returns \"darwin\" for macOS, \"linux\" for Linux.
  Throws ex-info for unsupported operating systems."
  []
  (let [os-name (System/getProperty "os.name")]
    (cond
      (re-find #"(?i)mac" os-name) "darwin"
      (re-find #"(?i)linux" os-name) "linux"
      :else (throw (ex-info (str "Unsupported OS: " os-name)
                            {:os os-name})))))

(defn detect-arch
  "Detect architecture.
  Returns \"aarch64\" for ARM64, \"x86_64\" for AMD64/x86-64.
  Throws ex-info for unsupported architectures."
  []
  (let [arch (System/getProperty "os.arch")]
    (case arch
      ("aarch64" "arm64") "aarch64"
      ("amd64" "x86_64") "x86_64"
      (throw (ex-info (str "Unsupported architecture: " arch)
                      {:arch arch})))))

(defn library-extension
  "Get platform-specific library file extension.
  Returns \".dylib\" for macOS, \".so\" for Linux."
  [os]
  (case os
    "darwin" ".dylib"
    "linux" ".so"))

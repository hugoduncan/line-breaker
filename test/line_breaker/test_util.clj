(ns line-breaker.test-util
  "Shared test utilities for line-breaker tests."
  (:require
   [babashka.fs :as fs]))

(defmacro with-temp-dir
  "Create a temporary directory, bind it to `sym`, execute `body`,
  then clean up the directory. Wrapper around fs/with-temp-dir
  that accepts the simpler [[sym] & body] syntax."
  [[sym] & body]
  `(fs/with-temp-dir [~sym {}]
     ~@body))

(defmacro with-captured-output
  "Capture stdout and stderr during body execution.
  Returns [out-str err-str result]."
  [& body]
  `(let [out# (java.io.StringWriter.)
         err# (java.io.StringWriter.)]
     (binding [*out* out#
               *err* err#]
       (let [result# (do ~@body)]
         [(str out#) (str err#) result#]))))

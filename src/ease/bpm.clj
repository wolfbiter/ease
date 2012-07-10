; Hack to allow clojure to run command-line commands.

(ns ease.cli
  (:use clojure.contrib.duck-streams))

(defn execute
  "Executes a command-line program, returning stdout if a zero return code, else the
   error out. Takes a list of strings which represent the command & arguments"
  [& args]
  (try
    (let [process (.exec (Runtime/getRuntime) (reduce str (interleave args (iterate str " "))))]
      (if (= 0 (.waitFor  process))
          (read-lines (.getInputStream process))
          (read-lines (.getErrorStream process))))
    (catch IOException ioe
      (throw (new RuntimeException (str "Cannot run" args)
ioe)))))

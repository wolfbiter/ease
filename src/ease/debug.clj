; Methods for debugging

(ns ease.debug)

(defmacro dprint
  "Debugging form that both prints out and returns results."
  [& more]
  `(let [start# ~more]
     (print '~more "==>" start# "\n")
     (println)
     start#))
(ns advent-of-code-2020.utilities
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn read-edn-file
  "Given resource file name, open and read."
  [name]
  (try
    (with-open [r (-> name io/resource io/reader)]
      (edn/read (java.io.PushbackReader. r)))

    (catch java.io.IOException e
      (printf "Couldn't open '%s': %s\n" name (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" name (.getMessage e)))))

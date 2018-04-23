(ns module-scanner.core
  (:gen-class)
  (:require [clojure.java.io]))

(def not-start (fn [line] (not= line "IMPORTANT_DICT = {")))
(def not-end (fn [line] (not= line "}")))

(defn- get-module-lines
  "Extract those lines which list the module dictionary entries"
  [lines]
  (->> lines
       (drop-while not-start)
       (take-while not-end)
       rest))

(defn -main
  "Make sure all of a Python package's modules are listed in a given
  configuration file"
  [& args]
  (let [config-file (first args)]
    (with-open [rdr (clojure.java.io/reader config-file)]
      (doall (map println (get-module-lines (line-seq rdr)))))))

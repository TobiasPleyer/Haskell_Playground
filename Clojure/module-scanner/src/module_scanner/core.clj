(ns module-scanner.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:use [clojure.java.shell :only [sh]]))

(def not-start (partial not= "IMPORTANT_DICT = {"))
(def not-end (partial not= "}"))
(def find-args "-type f -name *.py")

(defn- get-modules
  "Extract those lines which list the module dictionary entries"
  [lines]
  (->> lines
       (drop-while not-start)
       (take-while not-end)
       rest
       (map #(str/trim
               (first
                 (str/split % #":"))))
       (map #(str/replace % #"'" ""))))

(defn- mk-hierarchy
  "Splits a Python style include hierarchy string into its hierarchy
  components.
  Example 'path.to.package1' -> ['path' 'to' 'package1']"
  [path-string]
  (str/split path-string #"\."))

(defn- mk-path
  "Makes a Python inlcude path ('.' separated) to a UNIX file path
  ('/' separated"
  [path-string]
  (str/replace path-string #"\." "/"))


(defn -main
  "Make sure all of a Python package's modules are listed in a given
  configuration file"
  [& args]
  (let [package-dir (first args)
        config-file (str package-dir "/config.py")
        registered-modules (with-open [rdr (io/reader config-file)]
                             (reduce conj [] (map mk-path (get-modules (line-seq rdr)))))
        package-modules (:out (apply sh "find" package-dir (str/split find-args #" ")))]
    (println package-modules)
    (println registered-modules)
    (shutdown-agents)))

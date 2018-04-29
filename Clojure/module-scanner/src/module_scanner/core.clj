(ns module-scanner.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set])
  (:use [clojure.java.shell :only [sh]]))


(def not-start (partial not= "IMPORTANT_DICT = {"))
(def not-end (partial not= "}"))
(def find-args "-type f -name *.py")
;; When the blacklist and blacklist-functions are applied the Python files have
;; already been stripped of '.py' so don't assume '.py' to be present
(def blacklist-functions
  [(fn [module] (re-find #"__init__$" module))])
(def blacklist-func (apply juxt blacklist-functions))
(def blacklist
  ["resources/config"
   "resources/sub_pack_1/unimportant"
   "resources/safe"])


(defn in?
  "true if coll contains elm"
  [coll elm]
  (boolean (some #(= elm %) coll)))

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

(defn- get-want-set
  "Takes the path to the configuration file and returns the set of configured
  Python modules within that configuration."
  [config-file]
  (with-open [rdr (io/reader config-file)]
    (reduce conj #{} (map mk-path (get-modules (line-seq rdr))))))

(defn- blacklisted?
  [module]
  (let [bf (not (every? (comp false? boolean) (blacklist-func module)))
        bl (in? blacklist module)]
    (or bf bl)))

(defn- not-blacklisted?
  [module]
  (not (blacklisted? module)))

(defn- get-have-set
  "Takes the path to the Python package of interest and generates a set of
  contained modules, excluding all files for which a blacklist function returns
  true."
  [package-dir]
  (reduce conj #{}
          (filter not-blacklisted?
            (map #(str/replace % #"\.py$" "")
              (str/split-lines
                (:out (apply sh "find" package-dir (str/split find-args #" "))))))))


(defn -main
  "Make sure all of a Python package's modules are listed in a given
  configuration file"
  [& args]
  (let [package-dir (first args)
        config-file (str package-dir "/config.py")
        module-want-set (get-want-set config-file)
        module-have-set (get-have-set package-dir)
        missing-modules (set/difference module-have-set module-want-set)
        superfluous-modules (set/difference module-want-set module-have-set)]
    (if (every? true? [(empty? missing-modules)
                       (empty? superfluous-modules)])
        (println "Everything up-to-date")
        (do
          (println "Configuration out-of-date!")
          (if (not (empty? missing-modules))
              (do
                (println "The following modules are not listed in the configuration:")
                (doall (for [m missing-modules] (println (str "  - " m)))))
              nil)
          (if (not (empty? superfluous-modules))
              (do
                (println "The following modules do not exist any longer:")
                (doall (for [m superfluous-modules] (println (str "  - " m)))))
              nil)))
    (shutdown-agents)))

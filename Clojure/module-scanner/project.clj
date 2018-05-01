(defproject module-scanner "1.0.0"
  :description "Make sure a Python configuration is up-to-date"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot module-scanner.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

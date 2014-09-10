(defproject mlj "0.1.0-SNAPSHOT"
  :description "A ML dsl on top of Clojure"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [org.clojure/core.logic "0.8.8"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

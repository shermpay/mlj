(defproject mlj "0.1.0-SNAPSHOT"
  :description "A ML dsl on top of Clojure"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [org.clojure/core.logic "0.8.8"]
                 [instaparse "1.3.4"]
                 [rhizome "0.2.1"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

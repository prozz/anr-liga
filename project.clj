(defproject anr-liga "0.1.0-SNAPSHOT"
  :description "Netrunner League Management Tool"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.3.2"]]
  :main ^:skip-aot anr-liga.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

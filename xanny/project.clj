(defproject xanny "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clojure-opennlp "0.3.2"]
		  [clj-wordnet "0.1.0"]
                  [org.clojure/core.match "0.2.1"]]
  :main ^:skip-aot xanny.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})


(defproject utils "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.reader "1.0.0-beta4"]
                 [cheshire "5.7.0"]
                 [clj-time "0.13.0"]
                 [clj-http "3.5.0"]
                 [slingshot "0.12.2"]
                 [clj-jgit "0.8.9"]
                 [io.forward/yaml "1.0.6"]
                 [clojurewerkz/propertied "1.3.0"]
                 [version-clj "0.1.2"]
                 [k2nr/docker "0.0.2"]]
  :main ^:skip-aot utils.config
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

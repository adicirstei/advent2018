(defproject advent "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [proto-repl "0.3.1"]
                 [lein-light-nrepl "0.3.3"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [cljfmt "0.6.3"]]
  :plugins [[cider/cider-nrepl "0.19.0-SNAPSHOT"]
            [lein-cljfmt "0.6.3"]]
  :main ^:skip-aot advent.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

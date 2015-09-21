(defproject mujic "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.0.6"]
            [lein-figwheel "0.3.8-SNAPSHOT"]]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.137"]
                 [reagent "0.5.0"]]

  :source-paths ["src"]

  :cljsbuild {:builds {:dev {:source-paths ["src"]
                             :compiler {:output-to "mujic.js"
                                        :output-dir "target/dev"
                                        :asset-path "target/dev"
                                        :main "mujic"
                                        :optimizations :none
                                        :recompile-dependents true
                                        :source-map-timestamp true
                                        :source-map true}}
                       :gh-pages {:source-paths ["src"]
                                  :compiler {:output-to "public/mujic.js"
                                             :output-dir "target/gh"
                                             :asset-path "./"
                                             :main "mujic"
                                             :optimizations :advanced
                                             :recompile-dependents true}}}})

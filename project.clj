(defproject mujic "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.0.6"]
            [lein-figwheel "0.3.8-SNAPSHOT"]]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.122"]]

  :source-paths ["src"]

  :cljsbuild {:builds {:dev {:source-paths ["src"]
                             :figwheel true
                             :compiler {:output-to "mujic.js"
                                        :output-dir "out"
                                        :main "mujic"
                                        :asset-path "out"
                                        :optimizations :none
                                        :recompile-dependents true
                                        :source-map-timestamp true
                                        :source-map true}} }})

(defproject lseq-tree "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0-beta1"]
                 [org.clojure/clojurescript  "0.0-3211"]]

  :plugins  [[lein-cljsbuild  "1.0.5"]]

  :source-paths ["src"]
  :test-paths ["test/cljc"]
  :cljsbuild {:test-commands
              {"test" ["phantomjs"
                       "phantom/test.js"
                       "test.html"]}
              :builds [{:id "test"
                        :source-paths ["src" "test/cljc" "test/cljs"]
                        :compiler {:output-to "build/test/out.js"
                                   :output-dir "build/test/out"
                                   :cache-anlysis true
                                   :main lseq-tree-cljc.test-runner
                                   :optimizations :none}}]})

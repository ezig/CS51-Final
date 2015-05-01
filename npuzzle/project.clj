(defproject npuzzles "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
  			[com.taoensso/timbre "3.4.0"]
            [org.clojure/core.memoize "0.5.6"]
            [org.clojure/tools.cli "0.2.4"]
            [org.clojure/math.numeric-tower "0.0.4"]
            [org.clojure/data.priority-map "0.0.7"]]
  :main ^:skip-aot npuzzles.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

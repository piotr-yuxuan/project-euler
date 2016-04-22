(defproject project-euler "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/piotr-yuxuan/project-euler"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.logic "0.8.10"]]
  :main project-euler.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

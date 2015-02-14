(defproject bot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/java.jdbc "0.3.6"]
                 [mysql/mysql-connector-java "5.1.30"]
                 [twitter-api "0.7.8"]
                 [org.clojure/data.json "0.2.5"]]
  :main ^:skip-aot bot.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

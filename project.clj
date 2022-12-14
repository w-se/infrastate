(defproject org.clojars.w-se/infrastate "0.0.5"
  :description "Leightweight Infrastructure State Management Library"
  :url "http://github.com/w-se/infrastate"
  :license {:name "MIT" :url "https://www.mit.edu/~amini/LICENSE.md"}

  :repositories [["publish" {:url "https://clojars.org/repo"
                             :username :env/CLOJARS_USERNAME
                             :password :env/CLOJARS_PASSWORD
                             :sign-releases false}]]

   :dependencies [[org.clojure/clojure "1.11.1"]
                  [com.rpl/specter "1.1.4"]]
  :profiles {:dev { :dependencies [[nubank/matcher-combinators "3.5.0"]]}}
  :repl-options {:init-ns infrastate.core})

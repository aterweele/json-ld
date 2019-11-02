(defproject json-ld "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [better-cond "2.1.0"]  ; TODO move away from this
                 [name.atw/extensible-cond "0.1.0-SNAPSHOT"]
                 [clj-http "3.10.0"]
                 ;; I think I wanna replace this with
                 ;; clojure.data.json. This is for convenience.
                 [cheshire "5.9.0"]]
  :plugins [[lein-marginalia "0.9.1"]])

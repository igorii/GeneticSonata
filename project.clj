(defproject goldberg "0.1.0-SNAPSHOT"
  :description "The Goldberg Variations in Overtone."
  :main evol.core
  :dependencies	[
    [org.clojure/clojure "1.5.1"]
    [overtone "0.9.1" :exclusions [org.clojure/clojure]]
    [quil "1.6.0"]
    [leipzig "0.8.0"]])

(defproject genmusic "0.1.0-SNAPSHOT"
  :description "Evolution of music"
  :resource-paths ["resources/compojure.jar"]
  :profiles {:ga   {:main evol.core}
             :play {:main evol.play}
             :midi {:main evol.midi}}
  :aliases {"ga"   ["with-profile" "ga" "run"]
            "play" ["with-profile" "play" "run"]
            "midi" ["with-profile" "midi" "run"]}
  :dependencies	[
    [org.clojure/clojure "1.5.1"]
    [overtone "0.9.1" :exclusions [org.clojure/clojure]]
    [org.clojure/data.json "0.2.5"]
    [quil "1.6.0"]
    [leipzig "0.8.0"]])

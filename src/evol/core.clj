(ns evol.core
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        overtone.inst.sampled-piano
        [overtone.live :only [at ctl sample freesound-path]]))

(require '[leipzig.melody :refer [bpm is phrase then times where with]])
(require '[overtone.live :as overtone]
         '[leipzig.live :as live]
         '[leipzig.scale :as scale])

(require '[evol.fitness :as fitness])
(require '[evol.crossover :as crossover]) 
(require '[evol.selection :as selection])
(require '[evol.mutation :as mutation])
(require 'evol.utils)

;; The domain of a piece is its octave +/- half an octave
(def domain (into [] (range -4 13)))


;; Initialize one single line of music (one individual)
(defn init-one [hold-rate length domain]
  (map (fn [_]
         (if (> hold-rate (rand))
           HOLD
           (from-domain domain))) (range 0 length)))

(defn init-population [size hold-rate length domain]
  (map (fn [_] (init-one hold-rate length domain)) (range 0 size)))

(defn create-next-gen [fpop psize tsize]
  (map (fn [_] (apply crossover/one-point
                      (concat (map second (selection/tournament fpop tsize))
                              (list (rand-int (* 8 4))))))
       (range 0 (/ psize 2))))

;; Main
(defn -main []
  (defn l [iter oldpop]
    (let* [fits       (map (fitness/fitness 'theme E) oldpop)
           fpop       (map list fits oldpop)
           best       (max1 fpop)]
      (if (or = 0 (first best) (= 0 iter))
        best
        (recur (- iter 1) (map second (create-next-gen fpop 100 12))))))

  (let* [population (init-population 100 0.2 (* 8 4) domain)
         best (l 50 population)]
    (println "best: ")
    (println best)
    (play-one D major 130
              (mutation/sort-ascending
                (mutation/sort-descending
                  (mutation/sort-descending
                    (mutation/sort-ascending (second best) (* 8 4))
                    (* 8 4))
                  (* 8 4))
                (* 8 4)))))

(-main)

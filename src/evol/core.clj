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

(defn max1 [l]
  (reduce (fn [best x] (if (< (first x) (first best)) x best)) (first l) l))

;; Plays a single note
(defmethod play-note :default [{midi :pitch}] (sampled-piano midi))

;; Plays a list of notes for the given durations
(defn play-melody [key- mode bpm- pitches durations]
  (->> (phrase durations pitches)
    (where :part (is :melody))
    (where :time (bpm bpm-))
    (where :pitch (comp key- mode))
    play))

;; Return a single element renadomly from the domain
(defn from-domain [d] (d (rand-int (- (.length d) 1))))

;; Define fold holds
(defn fold-holds [melody lengths]
  (defn f [acc melody lengths]
    (if (empty? melody) acc
      (if (= (first melody) HOLD)
        (if (empty? acc)
          (f (cons (list (first melody) (first lengths)) acc) (rest melody) (rest lengths))
          (f (cons (list (first (first acc)) (+ 1/2 (second (first acc)))) (rest acc))
             (rest melody) (rest lengths)))
        (f (cons (list (first melody) (first lengths)) acc) (rest melody) (rest lengths)))))
  (f '() melody lengths))

;; Play a list of notes (currently each note is interpreted as an eigth note)
(defn play-one [key- mode bpm- melody]
  (let* [lengths (map (fn [_] 1/2) melody)
         both    (fold-holds melody lengths)
         new-melody (map first both)
         new-lengths (map second both)]
    (println both)
    (println new-melody)
    (println new-lengths)
    (play-melody key- mode bpm- (into [] new-melody) new-lengths)))

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


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
(require '[evol.utils :refer [HOLD ishold? get-notes]])

(defn max1 [l]
  (reduce (fn [best x] (if (< (first x) (first best)) x best)) (first l) l))

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
  (f '() (reverse melody) (reverse lengths)))

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

(defn chance [n] (< (rand) n))

(defn create-next-gen [fpop psize tsize strlen mutation%]
  (apply concat
    (map (fn [_] 
           (let [candidates (apply crossover/one-point
                                   (concat (map second (selection/tournament fpop tsize))
                                           (list (rand-int (* 8 4)))))]
             (println candidates)
             (if (chance mutation%) 
               (list ((mutation/random) (first  candidates) strlen)
                     ((mutation/random) (second candidates) strlen))
               candidates)))
         (range 0 (/ psize 2)))))

(defmethod play-note :default [{midi :pitch}] (sampled-piano midi))

;; Plays a list of notes for the given durations
(defn play-melody [key- mode bpm- pitches durations]
  (->> (phrase durations pitches)
    (where :part (is :melody))
    (where :time (bpm bpm-))
    (where :pitch (comp key- mode))
    play))

;; Play a list of notes (currently each note is interpreted as an eigth note)
(defn play-one [key- mode bpm- melody]
  (let* [lengths     (map (fn [_] 1/2) melody)
         both        (fold-holds melody lengths)
         new-melody  (map first both)
         new-lengths (map second both)]
    (println (into [] both))
    (play-melody key- mode bpm- (into [] new-melody) new-lengths)))

;; Main
(defn -main []
  (defn l [iter oldpop]
    (let* [fits       (map (fitness/fitness 'theme E) oldpop)
           fpop       (map list fits oldpop)
           best       (max1 fpop)]
      (println (first best))
      (if (or (= 0 (first best)) (= 0 iter))
        best
        (recur (- iter 1) (create-next-gen fpop 500 16 (* 8 4) 0.5)))))

  (let* [population (init-population 500 0.4 (* 8 4) domain)
         best (l 100 population)]
    (println "best: ")
    (println best)
    (play-one D major 130 (second best))))


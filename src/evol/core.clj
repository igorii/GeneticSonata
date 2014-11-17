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
(require '[evol.utils :refer :all])

(defn max1 [l]
  (reduce (fn [best x] (if (< (first x) (first best)) x best)) (first l) l))

;; Return a single element renadomly from the domain
(defn from-domain [d] (d (rand-int (- (.length d) 1))))

(defn zip [xs ys] (map list xs ys))

(defn fold-holds [melody lengths]

  (defn f [notes lengths noteacc lengthacc]
    (if (empty? notes)
      (zip noteacc lengthacc)
      (if (ishold? (first notes))
        (let [rlengths (reverse lengthacc)]
          (recur (rest notes) (rest lengths) noteacc (reverse (concat (list (+ 1/2 (first rlengths))) (rest rlengths)))))
          (recur (rest notes) (rest lengths) (concat noteacc (list (first notes))) (concat lengthacc (list 1/2))))))

  (f melody lengths '() '()))

(= (list '(1 1) '(3 1/2) '(4 1/2)) (fold-holds (list 1 HOLD 3 4) (list 1/2 1/2 1/2 1/2)))

(fold-holds (list 1 HOLD 3 4) (list 1/2 1/2 1/2 1/2))

;; The domain of a piece is its octave +/- half an octave
(def domain (into [] (range MINNOTE MAXNOTE)))

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
    (play-melody key- mode bpm- (into [] new-melody) new-lengths)))

(defn i->phrase [melody]
  (let* [lengths     (map (fn [_] 1/2) melody)
         both        (fold-holds melody lengths)
         new-melody  (map first both)
         new-lengths (map second both)]
    (phrase new-lengths new-melody)))

(defn play-sonata [key1- key2- mode- bpm- theme1 theme2 development tr1 tr2]
  (let [m1 (->> (i->phrase theme1) (where :pitch (comp key1- mode-)))
        m2 (->> (i->phrase theme2) (where :pitch (comp key2- mode-)))]
    (->>
      m1
      (then m2)
      (where :part (is :melody))
      (where :time (bpm bpm-))
      play)))

(defn -main []
  (defn l [iter oldpop strlen]
    (println (count oldpop))
    (let* [fits       (map (fitness/fitness 'theme E) oldpop)
           fpop       (map list fits oldpop)
           best       (max1 fpop)]
      (println (first best))
      (if (or (= 0 (first best)) (= 0 iter))
        (flatten (take 4 oldpop))
        (recur (- iter 1) (create-next-gen fpop 50 8 strlen 0.7) strlen))))

  (let* [population (init-population 50 0.7 8 domain)
         theme1 (l 100 population 8)
         theme2 (l 100 population 8)]
    (println theme1)
    (println theme2)
    (play-sonata C G major 100 theme1 theme2 nil nil nil)))

(defn play-random []
  (let [population (init-population 4 0.7 8 domain)]
    (play-one C major 100 (flatten population))))

(-main)


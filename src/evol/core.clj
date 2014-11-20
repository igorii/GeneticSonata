(ns evol.core
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        overtone.inst.sampled-piano
        [overtone.live :only [at ctl sample freesound-path]]))

(require '[leipzig.melody :refer [bpm is phrase then times where with]])
(require '[overtone.live :as overtone]
         '[leipzig.live :as live]
         '[leipzig.scale :as scale])
(require '[overtone.live :as olive])

(require '[evol.fitness :as fitness])
(require '[evol.crossover :as crossover])
(require '[evol.selection :as selection])

(require '[evol.mutation :as mutation])
(require '[evol.utils :refer :all])

(def PHRASELEN (* 8 2))
(def NOTEVAL 1/2)

(defn max1 [l]
  (reduce (fn [best x] (if (< (first x) (first best)) x best)) (first l) l))
(defn min1 [l]
  (reduce (fn [worst x] (if (> (first x) (first worst)) x worst)) (first l) l))

(defn zip [xs ys] (map list xs ys))

(defn fold-holds [melody lengths]
  (defn f [notes lengths noteacc lengthacc]
    (if (empty? notes)
      (zip noteacc lengthacc)
      (if (hold? (first notes))
        (let [rlengths (reverse lengthacc)]
          (recur (rest notes) (rest lengths) noteacc (reverse (concat (list (+ NOTEVAL (first rlengths))) (rest rlengths)))))
          (recur (rest notes) (rest lengths) (concat noteacc (list (first notes))) (concat lengthacc (list NOTEVAL))))))
  (println melody)
  (println lengths)
  (f melody lengths '() '()))

;; Initialize one single line of music (one individual)
(defn init-one [hold-rate length domain]
  (map (fn [_]
         (if (> hold-rate (rand))
           HOLD
           (from-domain domain))) (range 0 length)))

(defn init-population [size hold-rate length domain]
  (map (fn [_] (init-one hold-rate length domain)) (range 0 size)))

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

(olive/definst saw-wave [freq 440 attack 0.01 sustain 0.2 release 0.1 vol 0.4]
  (* (olive/env-gen (olive/env-lin attack sustain release) 1 1 0 1 olive/FREE)
     (olive/saw freq)
     vol))

; Slightly less to type
(saw-wave (note->hz :C5))

;; Let's make it even easier
(defn saw2 [music-note]
    (saw-wave (olive/midi->hz (olive/note music-note))))

;; Great!
(saw2 :A4)
(defmethod play-note :default [{midi :pitch}]

  (ctl sampled-piano :gate 0)
  ;(saw-wave (olive/midi->hz midi)))
  (sampled-piano midi))

;; Plays a list of notes for the given durations
(defn play-melody [key- mode bpm- pitches durations]
  (->> (phrase durations pitches)
    (where :part (is :melody))
    (where :time (bpm bpm-))
    (where :pitch (comp key- mode))
    play))

;; Play a list of notes (currently each note is interpreted as an eigth note)
(defn play-one [key- mode bpm- melody]
  (let* [lengths     (map (fn [_] NOTEVAL) melody)
         both        (fold-holds melody lengths)
         new-melody  (map first both)
         new-lengths (map second both)]
    (play-melody key- mode bpm- (into [] new-melody) new-lengths)))

(defn i->phrase [melody]
  (let* [lengths     (map (fn [_] NOTEVAL) melody)
         both        (fold-holds melody lengths)
         new-melody  (map first both)
         new-lengths (map second both)]
    (phrase new-lengths new-melody)))

(defn play-sonata [key1- key2- mode- bpm- theme1 theme2 development tr1 tr2]
  (let [m1 (->> (i->phrase theme1) (where :pitch (comp key1- mode-)))
        m2 (->> (i->phrase theme2) (where :pitch (comp key2- mode-)))
        m2' (->> (i->phrase theme2) (where :pitch (comp key1- mode-)))
        dev (->> (i->phrase development) (where :pitch (comp key2- mode-)))]
    (->>
      m1
      (then m2)
     (then dev)
     (then m1)
     (then m2')
     (then m1)
     (then m2')
      ;(then m1)
      ;(then m2)
      ;(then m2')
      ;(then m1)
      ;; repeat
      ;(then m1)
      ;(then m2')
      ;(then m1)
      ;(then m2)
      ;(then m2')
      ;(then m1)
      (where :part (is :melody))
      (where :time (bpm bpm-))
      play)))

(stop)
;(mooger 50)
;(assoc {} "hi" 5)
(defn -main []
  (defn l [iter oldpop strlen]
    (let* [fits       (map (fitness/fitness 'theme E) oldpop)
           fpop       (map list fits oldpop)
           best       (max1 fpop)]
           ;worst      (min1 fpop)]
      (println (first best))
      (if (or (= 0 (first best)) (= 0 iter))
        ;(flatten (take 4 oldpop))
        (second best)
        (recur (- iter 1) (cons (second best) (create-next-gen fpop 100 7 strlen 0.7)) strlen))))

  (let* [;population (init-population 100 0.4 PHRASELEN domain)
         theme1 (l 50 (init-population 100 0.4 PHRASELEN NOTERANGE) PHRASELEN)
         theme2 (l 50 (init-population 100 0.4 PHRASELEN NOTERANGE) PHRASELEN)
         init-measure-pop (concat (list (first-bar theme1))
                                  (list (last-bar theme1))
                                  (list (first-bar theme2))
                                  (list (last-bar theme2))
                                  (init-population 30 0.4 8 NOTERANGE))
         dev-measure-pop (refine-measure-pop init-measure-pop 44)
         development (development-from-pop dev-measure-pop 8)]

    (print-fitness-info theme1)
    (print-fitness-info theme2)
    (println dev-measure-pop)
    (play-sonata C G major 110 theme1 theme2 (flatten development) nil nil)))

(defn refine-measure-pop [init-pop popsize]
   (defn l [iter oldpop strlen]
    (let* [fits       (map (fitness/fitness 'development E) oldpop)
           fpop       (map list fits oldpop)]
      (if (= 0 iter)
        oldpop
        (recur (- iter 1) (create-next-gen fpop popsize 4 strlen 0.7) strlen))))
  (l 20 init-pop 8))

;(refine-measure-pop (list (list 0 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0 0)) 4)

(defn print-fitness-info [melody]
   (println melody)
    (print "  Start on tonic : ")
    (println (fitness/fit-start-on-tonic melody))
    (print "  End on tonic : ")
    (println (fitness/fit-end-on-tonic melody))
    (print "  Slope first half : ")
    (println (fitness/fit-slope-first-half melody))
    (print "  Slope second half : ")
    (println (fitness/fit-slope-second-half melody))
    (print "  Rest Ratio : ")
    (println (fitness/fit-rest-ratio melody))
    (print "  Note on beat : ")
    (println (fitness/fit-on-beat-notes melody))
    (print "  Melodic interval : ")
    (println (fitness/fit-melodic-intervals melody))
    (print "  Fit repeating patter : ")
    (println (fitness/fit-repeating-pattern melody 5)))

(defn development-from-pop [population dev-length]
  (map (fn [x] (first (shuffle population))) (range 0 dev-length)))

;(def t1 (list 0 3 0 -9 1 2 5 6 8 8 6 6 6 5 4 1))
;(def t2 (list 0 0 1 2 2 7 8 8 8 7 6 -9 -9 -9 5 1))
;(play-sonata C G major 110 t1 t2 nil nil nil)


;(define l (list 0 HOLD 1 HOLD HOLD 2 HOLD 3 HOLD 4 HOLD))

;(defn play-random []
;  (let [population (init-population 4 0.4 PHRASELEN NOTERANGE)]
;    (play-one C major 60 (first population))))


;(stop)
;(play-random)

(-main)

(sampled-piano (olive/note :C4))

;zipfs


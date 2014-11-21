(ns evol.core
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
       ; overtone.inst.sampled-piano
        overtone.inst.piano
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

(defn fold-holds [melody lengths]
  (defn f [notes lengths noteacc lengthacc]
    (if (empty? notes)
      (zip noteacc lengthacc)
      (if (hold? (first notes))
        (let [rlengths (reverse lengthacc)]
          (recur (rest notes) (rest lengths) noteacc (reverse (concat (list (+ NOTEVAL (first rlengths))) (rest rlengths)))))
          (recur (rest notes) (rest lengths) (concat noteacc (list (first notes))) (concat lengthacc (list NOTEVAL))))))
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

;; Plays a list of notes for the given durations
(defn play-melody [key- mode bpm- pitches durations]
  (->> (phrase durations pitches)
    (where :part (is :melody))
    (where :time (bpm bpm-))
    (where :pitch (comp key- mode))
    live/play))

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

(defn play-sonata [key1- key2- mode- bpm- chords theme1 theme2 development tr1 tr2]
  (let [m1  (->> (i->phrase theme1)      (where :pitch (comp key1- mode-)) (where :part (is :default)))
        m2  (->> (i->phrase theme2)      (where :pitch (comp key1- mode-)) (where :part (is :default)))
        m2' (->> (i->phrase theme2)      (where :pitch (comp key2- mode-)) (where :part (is :default)))
        dev (->> (i->phrase development) (where :pitch (comp key2- mode-)) (where :part (is :default)))
        bdev (flatten (repeat (/ (count dev) (count chords)) chords))
        bdev1 (->> (i->phrase bdev)                                         (where :part (is :bass)) (where :pitch (comp key2- mode-)))
        bdev2 (->> (i->phrase (map (fn [x] (if (hold? x) x (+ 3 x))) bdev)) (where :part (is :bass)) (where :pitch (comp key2- mode-)))
        bdev3 (->> (i->phrase (map (fn [x] (if (hold? x) x (+ 5 x))) bdev)) (where :part (is :bass)) (where :pitch (comp key2- mode-)))
        b1  (->> (i->phrase chords)                                         (where :part (is :bass)) (where :pitch (comp key1- mode-)))
        b2  (->> (i->phrase (map (fn [x] (if (hold? x) x (+ 3 x))) chords)) (where :part (is :bass)) (where :pitch (comp key1- mode-)))
        b3  (->> (i->phrase (map (fn [x] (if (hold? x) x (+ 5 x))) chords)) (where :part (is :bass)) (where :pitch (comp key1- mode-)))
        b1' (->> (i->phrase chords)                                         (where :part (is :bass)) (where :pitch (comp key2- mode-)))
        b2' (->> (i->phrase (map (fn [x] (if (hold? x) x (+ 3 x))) chords)) (where :part (is :bass)) (where :pitch (comp key2- mode-)))
        b3' (->> (i->phrase (map (fn [x] (if (hold? x) x (+ 5 x))) chords)) (where :part (is :bass)) (where :pitch (comp key2- mode-)))
        ]
    (println "Playing!")
    (println (count bdev))
    (println (count dev))
    (->>
     m1
      (then (with m1 b1 b2 b3 ))
      (then (with m1 b1 b2 b3 ))
      (then (with m2' b1' b2' b3' ))
      (then (with m2' b1' b2' b3' ))
      (then (with dev bdev1 bdev2 bdev3 ))
      (then (with m1 b1 b2 b3 ))
      (then (with m2 b1 b2 b3 ))
      (where :duration (bpm bpm-))
      (where :time (bpm bpm-))
      live/play)))

(olive/definst saw-wave [freq 440 attack 0.01 sustain 0.2 release 0.1 vol 0.4]
  (* (olive/env-gen (olive/env-lin attack sustain release) 1 1 0 1 olive/FREE)
     (olive/saw freq)
     vol))

(defn saw2 [music-note]
    (saw-wave (olive/midi->hz (olive/note music-note))))

(overtone/definst ping [freq 440]
  (-> freq
      overtone/square
      (* (overtone/env-gen (overtone/perc) :action overtone/FREE))))

(overtone/definst seeth [freq 440 dur 1.0]
  (-> freq
      overtone/saw
      (* (overtone/env-gen (overtone/perc (* dur 1/2) (* dur 1/2)) :action overtone/FREE))))

(overtone/definst beep [freq 440 dur 1.0]
  (-> freq
      overtone/saw
      (* (overtone/env-gen (overtone/perc 0.05 dur) :action overtone/FREE))))

;(beep 60)
;(saw2 60)
;(piano 60)
;(seeth 60)
;(ping 60)

(->>
 (phrase [1/2 1/2 1/2 1/2] [1 2 3 4])
 (where :part (is :default))
 (where :time (bpm 90))
 (where :duration (bpm 90))
 (where :pitch (comp C major))
 live/play)

(defmethod live/play-note :default [{midi :pitch seconds :duration}]
  (-> midi overtone/midi->hz beep))

(defmethod live/play-note :bass [{midi :pitch}]
  (-> midi overtone/midi->hz (/ 2) (seeth 1/2)))

(defn refine-measure-pop [init-pop popsize]
   (defn l [iter oldpop strlen]
    (let* [fits       (map (fitness/fitness 'development E) oldpop)
           fpop       (map list fits oldpop)]
      (if (= 0 iter)
        oldpop
        (recur (- iter 1) (create-next-gen fpop popsize 4 strlen 0.7) strlen))))
  (l 20 init-pop 8))

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

(defn -main []
  (defn l [iter oldpop strlen]
    (let* [fits       (map (fitness/fitness 'theme E) oldpop)
           fpop       (map list fits oldpop)
           best       (max1 fpop)]
      (println (first best))
      (if (or (= 0 (first best)) (= 0 iter))
        (second best)
        (recur (- iter 1
                  ) (cons (second best) (create-next-gen fpop 100 7 strlen 0.7)) strlen))))

  (let* [theme1 (l 50 (init-population 100 0.4 PHRASELEN NOTERANGE) PHRASELEN)
         theme2 (l 50 (init-population 100 0.4 PHRASELEN NOTERANGE) PHRASELEN)
         init-measure-pop (concat (list (first-bar theme1))
                                  (list (last-bar  theme1))
                                  (list (first-bar theme2))
                                  (list (last-bar  theme2))
                                  (init-population 30 0.4 8 NOTERANGE))
         dev-measure-pop (refine-measure-pop init-measure-pop 44)
         development     (development-from-pop dev-measure-pop 8)]

    (print-fitness-info theme1)
    (print-fitness-info theme2)
    (println dev-measure-pop)
    (play-sonata C G major 100
                 (flatten (map (fn [x] (cons x (repeat 3 HOLD))) CHORDS))
                 theme1 theme2 (flatten development) nil nil)))

;(sampled-piano (olive/note :c3))
;(sampled-piano (olive/note :e3))
;(sampled-piano (olive/note :g3))

;(sampled-piano (olive/note :a3))
;(sampled-piano (olive/note :c3))
;(sampled-piano (olive/note :e3))

;(sampled-piano (olive/note :b3))
;(sampled-piano (olive/note :d3))
;(sampled-piano (olive/note :f3))

(-main)
(stop)
;zipfs


(ns evol.core
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        overtone.inst.sampled-piano
        overtone.inst.synth))

(require '[leipzig.melody :refer [bpm is phrase then times where with]])

(require '[overtone.live :as overtone]
         '[leipzig.live :as live]
         '[leipzig.chord :as chord]
         '[leipzig.scale :as scale])

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
(defn init-one [hold-rate rest-rate length domain]
  (map (fn [_]
         (let [diceroll (rand)]
           (if (> hold-rate diceroll)
             HOLD
             (if (> (+ hold-rate rest-rate) diceroll)
               REST
               (from-domain domain))))) (range 0 length)))

(defn init-population [size hold-rate rest-rate length domain]
  (map (fn [_] (init-one hold-rate rest-rate length domain)) (range 0 size)))

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
  (let* [melody-with-rests (map (fn [x] (if (rest? x) x x)) melody)
         lengths     (map (fn [_] NOTEVAL) melody-with-rests)
         both        (fold-holds melody-with-rests lengths)
         new-melody  (map first both)
         new-lengths (map second both)]
    (println new-melody)
    (phrase new-lengths new-melody)))

#_(defn play-sonata [key1- key2- mode- bpm- chords theme1 theme2 development tr1 tr2]
  (println (melody->str (concat theme1 theme2 development tr1 tr2)))
  (let [m1  (->> (i->phrase theme1)      (where :pitch (comp key1- mode-)) (where :part (is :default)))
        m1' (->> (i->phrase theme1)      (where :pitch (comp key2- mode-)) (where :part (is :default)))
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
    (->>
      m1
      (then  (with m1 b1)); b2 b3 ))
      (then (with m2' b1')); b2 b3 ))
      (then (with m1' b1')); b2' b3' ))
      (then (with m2' b1')); b2' b3' ))
      ;(then dev)
      ;  (then (with dev (repeat b1'))); bdev1)); bdev2 bdev3 ))
      ;(then (with m1 b1)); b2)); b3))
      ;(then (with m2 b1)); b2)); b3))
      (where :duration (bpm bpm-))
      (where :time (bpm bpm-))
      live/play)))

(defn play-sonata [key1- key2- mode- bpm- chords theme1 theme2 development tr1 tr2]
  (println (melody->str (concat theme1 theme2 development tr1 tr2)))
  (let [m1  (->> (i->phrase theme1)      (where :pitch (comp key1- mode-)) (where :part (is :default)))
       ; m1' (->> (i->phrase theme1)      (where :pitch (comp key2- mode-)) (where :part (is :default)))
       ; m2  (->> (i->phrase theme2)      (where :pitch (comp key1- mode-)) (where :part (is :default)))
       ; m2' (->> (i->phrase theme2)      (where :pitch (comp key2- mode-)) (where :part (is :default)))
       ; dev (->> (i->phrase development) (where :pitch (comp key2- mode-)) (where :part (is :default)))
       ; bdev (flatten (repeat (/ (count dev) (count chords)) chords))
       ; bdev1 (->> (i->phrase bdev) (where :part (is :bass)) (where :pitch (comp key2- mode-)))
       ; bdev2 (->> (i->phrase bdev) (where :part (is :third)) (where :pitch (comp key2- mode-)))
       ; bdev3 (->> (i->phrase bdev) (where :part (is :fifth)) (where :pitch (comp key2- mode-)))
       ; b1  (->> (i->phrase chords) (where :part (is :bass)) (where :pitch (comp key1- mode-)))
       ; b2  (->> (i->phrase chords) (where :part (is :third)) (where :pitch (comp key1- mode-)))
       ; b3  (->> (i->phrase chords) (where :part (is :fifth)) (where :pitch (comp key1- mode-)))
       ; b1' (->> (i->phrase chords) (where :part (is :bass)) (where :pitch (comp key2- mode-)))
       ; b2' (->> (i->phrase chords) (where :part (is :third)) (where :pitch (comp key2- mode-)))
       ; b3' (->> (i->phrase chords) (where :part (is :fifth)) (where :pitch (comp key2- mode-)))
       ]
    (->>
      m1
      ;(then m1); b1)); b2 b3 ))
      ;(then m2'); b1')); b2' b3' ))
      ;(then m1'); b1')); b2' b3' ))
      ;(then m2'); b1')); b2' b3' ))
      ;(then dev)
      ;  (then (with dev (repeat b1'))); bdev1)); bdev2 bdev3 ))
      ;(then (with m1 b1)); b2)); b3))
      ;(then (with m2 b1)); b2)); b3))
      (where :duration (bpm bpm-))
      (where :time (bpm bpm-))
      live/play)))


;; INSTRUMENTS

(overtone/definst saw-wave [freq 440 attack 0.01 sustain 0.2 release 0.1 vol 0.4]
                  (* (overtone/env-gen (overtone/env-lin attack sustain release) 1 1 0 1 overtone/FREE)
                     (overtone/saw freq)
                     vol))

(defn saw2 [music-note]
  (saw-wave (overtone/midi->hz (overtone/note music-note))))

(overtone/definst seeth [freq 440 dur 1.0]
                  (-> freq
                    overtone/saw
                    (* (overtone/env-gen (overtone/perc (* dur 1/2) (* dur 1/2)) :action overtone/FREE))))

(overtone/definst beep [freq 440 dur 1.0]
                  (-> freq
                    overtone/saw
                    (* (overtone/env-gen (overtone/perc 0.05 dur) :action overtone/FREE))))

(overtone/definst organ [freq 440 dur 1 land 0.9 volume 1]
                  (-> (overtone/square freq)
                    (+ (overtone/sin-osc (* 3 freq) (overtone/sin-osc 6)))
                    (+ (overtone/sin-osc (* 1/2 freq) (overtone/sin-osc 3)))
                    (* (overtone/env-gen (overtone/adsr 0.03 0.3 0.4) (overtone/line:kr 1 0 dur) :action overtone/FREE))
                    (* (overtone/sin-osc (* freq 2)))
                    (overtone/clip2 (overtone/line:kr 1 land 16))
                    (* volume)))

(overtone/definst plink [freq 440 dur 1 volume 1.0]
                  (-> (overtone/sin-osc freq)
                    (+ (* 1/3 (overtone/sin-osc (* freq 3))))
                    (+ (* 1/5 (overtone/sin-osc (* freq 5.1))))
                    (+ (* 1/6 (overtone/sin-osc (* freq 6.1))))
                    (+ (* 1/8 (overtone/sin-osc (* freq 7.1))))
                    (+ (* 1/8 (overtone/sin-osc (* freq 8))))
                    (* (overtone/env-gen (overtone/perc 0.01 0.4) :action overtone/FREE))
                    (* volume)))

(overtone/definst bass2 [freq 110 dur 1 volume 1.0]
                  (-> (overtone/saw freq)
                    (overtone/rlpf (overtone/line:kr 2000 freq dur) 0.5)
                    (* (overtone/env-gen (overtone/perc 0.1 dur) :action overtone/FREE))
                    (* volume)))

(defn play-chord [chord]
  (doseq [note chord] (plink note)))

(defn note-distribution [melody]
  (let [notes (get-notes melody)]
    (sort (reduce (fn [acc x]
                    (assoc acc (count x) (+ 1 (get acc (count x) 0))))
                  {} notes))))

(defn note-frequency [melody]
  (let [total (count (get-notes melody))]
    (map (fn [x] [(first x) (/ (second x) total)]) (note-distribution melody))))

(defmethod live/play-note :default [{midi :pitch seconds :duration}]
  (when midi (-> (overtone/midi->hz midi) (seeth :dur seconds))))

(defmethod live/play-note :default [{midi :pitch seconds :duration}]
  (when midi
    (overtone/ctl sampled-piano :gate 0)
    (-> midi sampled-piano)));(overtone/midi->hz midi) (* 81) (/ 64) (organ :dur seconds))))

(defmethod live/play-note :fifth [{midi :pitch seconds :duration}]
  (when midi (-> (overtone/midi->hz midi) (* 3) (/ 2) (organ :dur seconds))))

(defmethod live/play-note :bass [{midi :pitch seconds :duration}]
  (when midi (-> (overtone/midi->hz midi) (/ 2) (organ :dur seconds :volume 0.55))))

(defn refine-measure-pop [init-pop popsize]
  (defn l [iter oldpop strlen]
    (let* [fits       (map (fitness/fitness 'development E) oldpop)
           fpop       (map list fits oldpop)]
      (if (= 0 iter)
        oldpop
        (recur (- iter 1) (create-next-gen fpop popsize 4 strlen 0.9) strlen))))
  (l 10 init-pop 8))

(defn development-from-pop [population dev-length]
  (map (fn [x] (first (shuffle population))) (range 0 dev-length)))

(defn -main []
  (defn l [iter oldpop strlen]
    (let* [fits       (map (fitness/fitness 'theme E) oldpop)
           fpop       (map list fits oldpop)
           best       (max1 fpop)]
      ;(print iter)
      ;    (print " ")
      ;(print (first best))
      ;    (print "   ")
      ;    (print (fitness/get-interval-frequencies (second best)))
      ;    (println)
      (if (or (= 0 (first best)) (= 0 iter))
        (second best)
        (recur (- iter 1)
               (cons (second best) (create-next-gen fpop 500 4 strlen 0.3))
               strlen))))

  (let* [theme1 (l 100 (init-population 500 0.4 0 PHRASELEN NOTERANGE) PHRASELEN)
       ];  theme2 (l 50 (init-population 100 0.4 0 PHRASELEN NOTERANGE) PHRASELEN)
       ;  init-measure-pop (concat (list (first-bar theme1))
       ;                           (list (last-bar  theme1))
       ;                           (list (first-bar theme2))
       ;                           (list (last-bar  theme2))
       ;                           (init-population 30 0.4 0 8 NOTERANGE))
       ;  dev-measure-pop (refine-measure-pop init-measure-pop 34)
       ;  development     (development-from-pop dev-measure-pop 8)]
        (println)
        (println)
        (print (fitness/get-interval-frequencies theme1)) (println)
        (print (fitness/get-length-frequencies theme1))   (println)
        (println theme1)
        (recur)))
    ;(play-sonata C G major 100 nil theme1 nil nil nil nil)))
                ; (flatten (map (fn [x] (cons x (repeat 3 HOLD))) CHORDS))
                ; theme1 theme2 (flatten development) nil nil)))

#_(->>
  (with (->> (phrase (repeat 2) [0 1 2 3 4 5 6 7]) (where :part (is :default)))
        (->> (phrase (repeat 2) [0 1 2 3 4 5 6 7]) (where :part (is :bass))))
  (where :time (bpm 90))
  (where :duration (bpm 90))
  (where :pitch (comp C major))
  live/play)
#_(stop)

(stop)

(-main)




(def t
  (list 0 -1 2 3 -9 -9 3 -7 3 -9 -2 2 -6 -3 -7 -9 8 6 -7 -9 -9 0 -4 4 2 -9 4 -9 -9 -9 7 2 2 -4 4 5 -9 -5 -9 -2 5 1 8 -9 6 -9 -9 -5 -3 4 2 -9 8 -1 6 -9 -2 -9 -9 -9 -9 -9 -9 0)

  )
(play-sonata C G major 100 nil t nil nil nil nil)


;zipfs
;(i->phrase (init-one 0.4 0.2 16 [0]))

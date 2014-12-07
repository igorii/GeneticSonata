(ns evol.core
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        overtone.inst.sampled-piano
        overtone.inst.piano
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
      (then (with m1 b1)); b2 b3 ))
      (then (with chords m2' b1')); b2 b3 ))
      (then (with chords m1' b1')); b2' b3' ))
      (then (with m2' b1')); b2' b3' ))
      ;(then dev)
      ;  (then (with dev (repeat b1'))); bdev1)); bdev2 bdev3 ))
      ;(then (with m1 b1)); b2)); b3))
      ;(then (with m2 b1)); b2)); b3))
      (where :duration (bpm bpm-))
      (where :time (bpm bpm-))
      live/play)))

(defn make-chords [roots key- mode-]
  (let [fold (fold-holds  roots (map (fn [_] NOTEVAL) roots))]
  (->> (phrase (map second fold)
               (into [] (map (fn [x] (-> chord/triad (chord/root x) (dissoc :v))) (map first fold))))
       (wherever :pitch, :pitch lower)
       (where :pitch (comp key- mode-))
       (where :part (is :default)))))

(make-chords (list 0 HOLD 1 HOLD HOLD 2 3) C major)
(play-sonata C G major 80 nil t nil nil nil nil)
(stop)

(fold-holds (list 0 HOLD 1 HOLD 2 HOLD HOLD 3)(list 1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2) )

chords
(play-sonata C G major 80 (second s) (first s) nil nil nil nil)

(stop)

(flatten (repeat 5 (list 1 2)))

(defn play-sonata [key1- key2- mode- bpm- chords1 theme1 chords2 theme2 development tr1 tr2]
  (println (melody->str (concat theme1 theme2 development tr1 tr2)))
  (let* [m1  (->> (i->phrase theme1) (where :pitch (comp key1- mode-)) (where :part (is :default)))
        m1'  (->> (i->phrase theme1) (where :pitch (comp key2- minor)) (where :part (is :default)))
        m2   (->> (i->phrase theme2) (where :pitch (comp key1- mode-)) (where :part (is :default)))
        m2'  (->> (i->phrase theme2) (where :pitch (comp key2- minor)) (where :part (is :default)))
        dev' (->> (i->phrase development) (where :pitch (comp key2- minor)) (where :part (is :default)))
        nchords1  (make-chords chords1 key1- mode-)
        nchords1' (make-chords chords1 key2- minor)
        nchords2  (make-chords chords2 key1- mode-)
        nchords2' (make-chords chords2 key2- minor)
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
      (then (with m1   nchords1))
      (then (with m2'  nchords2'))
      (then (with m1'  nchords1'))
      (then (with m2'  nchords2'))
      (then dev')
      (then (with m1   nchords1))
      (then (with m2   nchords2))
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
  (when midi
    ;(overtone/ctl sampled-piano :gate 0)
    (-> midi sampled-piano)));(overtone/midi->hz midi) (* 81) (/ 64) (organ :dur seconds))))

(defmethod live/play-note :fifth [{midi :pitch seconds :duration}]
  (when midi (-> (overtone/midi->hz midi) (* 3) (/ 2) (organ :dur seconds))))

(defmethod live/play-note :bass [{midi :pitch seconds :duration}]
  (when midi (-> (overtone/midi->hz midi) (/ 2) (beep :dur seconds :volume 0.55))))

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

(def last-song (ref nil))

(defn -main []
  (defn l [iter oldthemepop oldchordpop strlen]
    (let* [fits-themes (map (fitness/fitness 'theme C) oldthemepop)
           fpop-themes (map list fits-themes oldthemepop)
           fits-chords (map (fitness/fitness 'chord C) oldchordpop)
           fpop-chords (map list fits-chords oldchordpop)
           best-theme  (max1 fpop-themes)
           best-chord  (max1 fpop-chords)
           ]
      (print iter)
      (print " ")
      (println [(first best-theme) (first best-chord)])
      ;    (print "   ")
      ;    (print (fitness/get-interval-frequencies (second best)))
      ;    (println)
      (if (= 0 iter)
        [(into [] (second best-theme))
         (into [] (second best-chord))]
        (recur (- iter 1)
               (cons (second best-theme) (create-next-gen fpop-themes 500 4 strlen 0.3))
               (cons (second best-chord) (create-next-gen fpop-chords 500 4 strlen 0.3))
               strlen))))

  (let* [theme1-pop  (init-population 500 0.4 0 PHRASELEN NOTERANGE)
         chord1-pop  (init-population 500 0.6 0 PHRASELEN CHORDRANGE)
         theme2-pop  (init-population 500 0.4 0 PHRASELEN NOTERANGE)
         chord2-pop  (init-population 500 0.6 0 PHRASELEN CHORDRANGE)
         theme1 (l 1000 theme1-pop chord1-pop PHRASELEN)
         theme2 (l 1000 theme2-pop chord2-pop PHRASELEN)
       ;  theme2 (l 50 (init-population 100 0.4 0 PHRASELEN NOTERANGE) PHRASELEN)
         init-measure-pop (concat (list (first-bar (first theme1)))
                                  (list (last-bar  (first theme1)))
                                  (list (first-bar (first theme2)))
                                  (list (last-bar  (first theme2)))
                                  (init-population 30 0.4 0 8 NOTERANGE))
         dev-measure-pop (refine-measure-pop init-measure-pop 34)
         development     (development-from-pop dev-measure-pop 8)
        ];(println)
        (println development)
        ;(print (fitness/get-interval-frequencies theme1)) (println)
        ;(print (fitness/get-length-frequencies theme1))   (println)
        (println theme1) (println theme2)
        (dosync (ref-set last-song [theme1 theme2 (flatten development)]))

        (spit "event.log"
              (str "\n\n"
                   (str [theme1 theme2 (flatten development)])) :append true)
        (recur)))
        ;     (playy [theme1 theme2 (flatten development)])))
       ; (fitness/print-fitness-info theme1)
       ; (recur)))
    ;(play-sonata C G major 100 nil theme1 nil nil nil nil)))
                ; (flatten (map (fn [x] (cons x (repeat 3 HOLD))) CHORDS))
                ; theme1 theme2 (flatten development) nil nil)))
(stop)
#_(->>
  (with (->> (phrase (repeat 2) [0 1 2 3 4 5 6 7]) (where :part (is :default)))
        (->> (phrase (repeat 2) [0 1 2 3 4 5 6 7]) (where :part (is :bass))))
  (where :time (bpm 90))
  (where :duration (bpm 90))
  (where :pitch (comp C major))
  live/play)
#_(stop)

(stop)

(fitness/get-length-frequencies (first s))

(-main)

(str (str (deref last-song)))
;(playy
 (deref last-song);)
(stop)

(require '[leipzig.chord :as chord])
(->>
  chords
  (where :time (bpm 90))
  (where :duration (bpm 90))
  (wherever :pitch, :pitch (comp scale/C scale/major))
  live/play)

(stop)

(fitness/get-length-frequencies t)
(fitness/get-interval-frequencies t)

(def mozartssadness
 [[[0 -9 3 -9 -9 4 5 -9 7 -9 -9 6 4 -9 -9 -9 0 -4 0 1 6 6 -9 7 7 6 6 -9 -9 2 -3 0] [0 -9 -9 -9 3 -9 -9 -9 6 -9 -9 1 3 -9 -9 -9 4 -9 2 -9 1 -9 -9 -9 3 -9 -9 -9 2 3 4 0]]
  [[0 -9 3 -9 -9 4 5 -9 7 -9 -9 6 4 -9 -9 -9 0 -4 0 1 6 6 -9 7 7 6 6 -9 -9 2 -3 0] [0 -9 -9 -9 3 -9 -9 -9 6 -9 -9 1 3 -9 -9 -9 4 -9 2 -9 1 -9 -9 -9 3 -9 -9 -9 2 3 4 0]]
  ]
  )

(def song2
  [[[-1 -9 4 -9 -9 3 4 -9 8 6 8 -9 -9 -9 5 3 3 4 5 5 -9 2 1 6 7 6 5 3 5 -9 1 0] [1 5 0 -9 1 -9 -9 -9 -3 -9 -9 -9 2 -9 -9 -9 -1 -9 3 -9 -9 -9 2 -9 1 -9 -9 3 -9 -9 -9 1]] [[3 8 -9 -9 7 4 8 7 8 -9 7 -9 -9 -9 8 8 6 -9 7 -9 8 -9 8 -9 6 -9 5 -9 3 5 2 0] [1 -9 -9 -9 2 -9 -9 -9 0 -9 -9 -9 4 -9 -9 2 1 -9 -9 -9 2 -9 0 -9 5 -9 -9 -9 3 -9 -9 0]]]
)

(def song3
  [[[0 -9 5 -9 3 4 -9 -9 3 -9 -9 -9 5 4 5 -9 3 4 4 3 5 -9 4 5 3 2 3 0 4 -9 5 0] [0 -9 -9 -9 1 -9 -9 -9 3 -9 -9 -9 4 -9 -9 -9 3 -9 6 -9 -9 -9 1 -9 3 -9 -9 -9 4 -9 -9 0]] [[0 4 3 -9 2 4 5 -9 3 2 4 -9 5 -9 -9 0 3 -9 -9 -9 2 -9 4 6 6 -9 4 3 2 -9 1 0] [0 -9 1 -9 -2 -9 -9 -9 3 -9 -9 -9 5 -9 -9 -9 4 -9 -9 -9 3 -9 -9 -9 5 -9 -9 -9 4 -9 -9 0]] (list 3 -9 5 -9 3 -9 2 -4 0 -9 2 -1 -9 8 -9 3 7 -9 8 -9 6 -9 8 0 -1 -9 6 -9 6 -9 3 -4 3 -9 -9 -9 3 7 8 -9 -1 8 -9 6 6 -7 6 2 0 -9 2 -1 -9 8 -9 3 8 -9 7 3 1 1 0 8)]
)

(def song4
  [[[0 -9 3 5 4 5 1 0 5 4 8 -9 3 -9 -9 2 6 -9 -9 -9 4 4 0 -1 4 -9 4 -9 0 -9 -2 0] [0 -9 -9 -9 1 -9 -9 -9 2 -9 -9 -9 6 -9 -9 -9 8 -9 3 -9 2 -9 -9 -9 1 -9 -9 -9 3 -9 -9 0]] [[0 5 4 -9 2 0 2 -9 3 4 3 2 3 -9 4 3 2 3 8 -9 4 2 -9 -9 1 -9 -9 -9 3 -9 0 0] [0 -9 -9 -9 -1 -9 -9 -9 4 -9 -9 -9 8 -9 -9 -9 6 -9 -9 -9 5 -9 3 -9 4 -9 -9 -9 3 -9 -9 0]] (list 5 -1 1 -1 -3 -1 0 -9 -4 -4 -2 -1 -9 -9 -3 -2 7 8 8 8 -9 -9 8 8 7 -9 8 -9 -9 -9 2 7 -7 -7 -7 -6 0 -3 -9 -1 0 -3 -9 -1 -7 -7 -6 -9 3 1 -4 -3 -3 -9 0 1 0 -1 -9 -9 -6 -9 -7 -9)]
)
((fitness/fitness 'theme C) (first mozartssadness))

(fitness/print-fitness-info (first mozartssadness))

(playy song4)

(def s1
  [[0 5 6 -9 8 5 3 -9 1 2 -9 3 -9 -9 4 -9 2 1 0 4 5 -9 -9 -9 4 -9 3 4 -9 -9 2 0] [0 4 5 -9 -9 -9 8 -9 3 -9 -9 -9 2 -9 -9 -9 4 -9 -9 -9 2 -9 -9 -9 1 -9 -9 -9 2 -9 -9 0]]
  )

(def s2
  [[0 -9 1 -9 6 7 3 2 3 0 2 -9 1 3 -9 1 0 -4 -5 -7 -9 -9 -7 -4 -1 0 0 -9 -9 -9 2 0] [0 -9 5 -9 -9 -9 4 -9 5 8 -9 -9 -9 4 -9 -9 5 -9 -9 -9 7 -9 -9 -9 5 -9 4 -9 -9 -9 0 0]]
  )

(println (deref last-song))

(defn inst-chords [theme chords]
  (reduce (fn [acc x]
            (if (hold? (second x))
              (concat acc (list (second x)))
              (concat acc (list (first x)))))
          () (zip theme chords)))

;[key1- key2- mode- bpm- chords theme1 theme2 development tr1 tr2]
 (playy (deref last-song))

(defn playy [s]
  (let [s1 (first s)
        s2 (second s)
        dev (second (rest s))]
    (play-sonata C G major 80
       (inst-chords (first s1) (second s1))
                 (first s1)
             (inst-chords (first s2) (second s2))
                 (first s2)
                 dev nil nil)))

(playy song2)
(first s)

(stop)

(let [chord-notes (map first (partition 4 (first mozartssadness)))]
  (flatten (interleave chord-notes (repeat (count chord-notes) (list (list HOLD HOLD HOLD))))))


;zipfs
;(i->phrase (init-one 0.4 0.2 16 [0]))

(ns evol.core
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
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


(def dull-partials
  [
   0.56
   0.92
   1.19
   1.71
   2
   2.74
   3
   3.76
   4.07])

;; http://www.soundonsound.com/sos/Aug02/articles/synthsecrets0802.asp
;; (fig 8)
(def partials
  [
   0.5
   1
   3
   4.2
   5.4
   6.8])

;; we make a bell by combining a set of sine waves at the given
;; proportions of the frequency. Technically not really partials
;; as for the 'pretty bell' I stuck mainly with harmonics.
;; Each partial is mixed down proportional to its number - so 1 is
;; louder than 6. Higher partials are also supposed to attenuate
;; quicker but setting the release didn't appear to do much.

(overtone/defcgen bell-partials
  "Bell partial generator"
  [freq {:default 440 :doc "The fundamental frequency for the partials"}
   dur  {:default 1.0 :doc "Duration multiplier. Length of longest partial will
                            be dur seconds"}
   partials {:default [0.5 1 2 4] :doc "sequence of frequencies which are
                                        multiples of freq"}]
  "Generates a series of progressively shorter and quieter enveloped sine waves
  for each of the partials specified. The length of the envolope is proportional
  to dur and the fundamental frequency is specified with freq."
  (:ar
   (apply +
          (map
           (fn [partial proportion]
             (let [env      (overtone/env-gen (overtone/perc 0.01 (* dur proportion)))
                   vol      (/ proportion 2)
                   overtone (* partial freq)]
               (* env vol (overtone/sin-osc overtone))))
           partials ;; current partial
           (iterate #(/ % 2) 1.0)  ;; proportions (1.0  0.5 0.25)  etc
           ))))

(defn fold-holds [melody lengths]
  (defn f [notes lengths noteacc lengthacc]
    (if (empty? notes)
      (zip noteacc lengthacc)
      (if (hold? (first notes))
        (let [rlengths (reverse lengthacc)]
          (recur (rest notes) (rest lengths) noteacc (reverse (concat (list (+ NOTEVAL (first rlengths))) (rest rlengths)))))
          (recur (rest notes) (rest lengths) (concat noteacc (list (first notes))) (concat lengthacc (list NOTEVAL))))))
  (f melody lengths '() '()))

;(stop)
;(ks-stringer 60)

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
    (println "Playing!")
    (println (count bdev))
    (println (count dev))
    (->>
     m1
      (then m1); (with m1 b1)); b2 b3 ))
      (then m2'); (with m2' b1')); b2 b3 ))
      (then m1'); (with m1' b1')); b2' b3' ))
      (then m2'); (with m2' b1')); b2' b3' ))
      (then dev); (with dev bdev1)); bdev2 bdev3 ))
      (then m1); (with m1 b1)); b2)); b3))
      (then m2); (with m2 b1)); b2)); b3))
      (where :duration (bpm bpm-))
      (where :time (bpm bpm-))
      live/play)))

(overtone/definst saw-wave [freq 440 attack 0.01 sustain 0.2 release 0.1 vol 0.4]
  (* (overtone/env-gen (overtone/env-lin attack sustain release) 1 1 0 1 overtone/FREE)
     (overtone/saw freq)
     vol))

(defn saw2 [music-note]
    (saw-wave (overtone/midi->hz (overtone/note music-note))))

;(overtone/definst ping [freq 440]
;  (-> freq
;      overtone/square
;      (* (overtone/env-gen (overtone/perc) :action overtone/FREE))))

(overtone/definst seeth [freq 440 dur 1.0]
  (-> freq
      overtone/saw
      (* (overtone/env-gen (overtone/perc (* dur 1/2) (* dur 1/2)) :action overtone/FREE))))

(overtone/definst beep [freq 440 dur 1.0]
  (-> freq
      overtone/saw
      (* (overtone/env-gen (overtone/perc 0.05 dur) :action overtone/FREE))))

(overtone/definst dull-bell [freq 220 dur 1.0 amp 1.0]
  (let [snd (* amp (bell-partials freq dur dull-partials))]
    ;(overtone/detect-silence snd :action overtone/FREE)
    snd))

(overtone/definst pretty-bell [freq 220 dur 1.0 amp 1.0]
  (let [snd (* amp (bell-partials freq dur partials))]
    (overtone/detect-silence snd :action overtone/FREE)
    snd))

(dull-bell)
(pretty-bell (overtone/note :D2))

;(beep 60)
;(saw2 60)
;(piano 60)
;(seeth 60)
;(ping 60)
;(simple-flute)
;(cs80lead)

(->>
 (with (->> (phrase [1/2 1/2 1/2 1/2] [0 3 5 0]) (where :part (is :default)))
 (->> (phrase [1 1] [0 0]) (where :part (is :bass))))
 (where :time (bpm 90))
 (where :duration (bpm 90))
 (where :pitch (comp C major))
 live/play)
(stop)

(defmethod live/play-note :default [{midi :pitch seconds :duration}]
  (-> midi overtone/midi->hz (organ :volume 0.7 :dur seconds)))

(defmethod live/play-note :bass [{midi :pitch seconds :duration}]
  (-> midi overtone/midi->hz (/ 2) (organ :volume 0.55 :dur seconds)))

(defn refine-measure-pop [init-pop popsize]
   (defn l [iter oldpop strlen]
    (let* [fits       (map (fitness/fitness 'development E) oldpop)
           fpop       (map list fits oldpop)]
      (if (= 0 iter)
        oldpop
        (recur (- iter 1) (create-next-gen fpop popsize 4 strlen 0.9) strlen))))
  (l 10 init-pop 8))

(defn print-fitness-info [melody]
   (print-melody melody)
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
         dev-measure-pop (refine-measure-pop init-measure-pop 34)
         development     (development-from-pop dev-measure-pop 8)]

    (print-fitness-info theme1)
    (print-fitness-info theme2)
    (println dev-measure-pop)
    (play-sonata C G major 100
                 (flatten (map (fn [x] (cons x (repeat 3 HOLD))) CHORDS))
                 theme1 theme2 (flatten development) nil nil)))


;(sampled-piano (overtone/note :c3))
;(sampled-piano (overtone/note :e3))
;(sampled-piano (overtone/note :g3))

;(sampled-piano (overtone/note :a3))
;(sampled-piano (overtone/note :c3))
;(sampled-piano (overtone/note :e3))

;(sampled-piano (overtone/note :b3))
;(sampled-piano (overtone/note :d3))
;(sampled-piano (overtone/note :f3))

;(play-one C major 100 (list 1 HOLD 2 HOLD 3 HOLD 4 HOLD))
;(play-one C minor 100 (list 6 5 6 HOLD 2 HOLD 1 HOLD 6 5 6 HOLD 2 HOLD 1 HOLD 6 5 6 HOLD HOLD HOLD 2 HOLD 0 HOLD 1 HOLD HOLD HOLD))
;(play-one C minor 100 (list 8 8 9 HOLD 5 HOLD 4 HOLD 6 5 6 HOLD 2 HOLD 1 HOLD 6 5 6 HOLD HOLD HOLD 2 HOLD 0 HOLD 1 HOLD HOLD HOLD))

;((fn [] (let [h HOLD] (play-one C minor 100 (list 0 h 1 h 2 h 3 h 4 h)))))

(saw2 60)
(doseq [note (overtone/chord :E3 :major7)] (piano note))

(defn play-chord [chord]
  (doseq [note chord] (plink note)))

(play-chord (overtone/chord :C4 :major))
(let [time (overtone/now)]
  (overtone/at time (play-chord (overtone/chord :C3 :major)))
  (overtone/at (+ 1000 time) (play-chord (overtone/chord :C3 :major7)))
  (overtone/at (+ 2000 time) (play-chord (overtone/chord :E3 :minor)))
  (overtone/at (+ 3000 time) (play-chord (overtone/chord :A2 :minor))))

(map organ [60 63 65 67])
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

(organ (overtone/note :C4))

(-main)
(stop)



(import '(javax.swing JFrame JLabel JTextField JButton)
        '(java.awt.event ActionListener)
        '(java.awt GridLayout))

(let [frame (new JFrame "Celsius Converter")
      temp-text (new JTextField)
      celsius-label (new JLabel "Celsius")
      convert-button (new JButton "Convert")
      fahrenheit-label (new JLabel "Fahrenheit")]
    (. convert-button
        (addActionListener
           (proxy [ActionListener] []
                (actionPerformed [evt]
                    (let [c (Double/parseDouble (. temp-text (getText)))]
                      (. fahrenheit-label
                         (setText (str (+ 32 (* 1.8 c)) " Fahrenheit"))))))))
    (doto frame
                ;(.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE)) ;uncomment this line to quit app on frame close
                (.setLayout (new GridLayout 2 2 3 3))
                (.add temp-text)
                (.add celsius-label)
                (.add convert-button)
                (.add fahrenheit-label)
                (.setSize 600 180)
                (.setVisible true)))




((organ :dur 1/2) 60)

;zipfs
(+ 1 2)

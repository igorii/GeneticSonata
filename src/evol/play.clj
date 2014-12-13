(ns evol.play
  (:gen-class)
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        overtone.inst.sampled-piano
        overtone.inst.synth))

(require '[overtone.live :as overtone]
         '[leipzig.chord :as chord]
         '[leipzig.melody :refer [bpm is phrase then times where with]])

(require '[evol.utils :refer :all])

;; ***********
;; INSTRUMENTS
;; ***********

(overtone/definst saw-wave [freq 440 attack 0.01 sustain 0.2 release 0.1 vol 0.4]
                  (* (overtone/env-gen (overtone/env-lin attack sustain release) 1 1 0 1 overtone/FREE)
                     (overtone/saw freq)
                     vol))

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

;; Play a single chord
(defn play-chord [chord]
  (doseq [note chord] (plink note)))

;; Play a single note as a sampled piano
(defmethod play-note :default [{midi :pitch seconds :duration}]
 ;(set :gate sampled-piano 0)
  (when midi (-> midi sampled-piano)))

;; ***********
;;    PLAY
;; ***********

(defn play-song [s settings]

  ;; Play a list of notes (currently each note is interpreted as an eigth note)
  (defn play-one [key- mode bpm- melody]

    ;; Plays a list of notes for the given durations
    (defn play-melody [key- mode bpm- pitches durations]
      (->> (phrase durations pitches)
        (where :part (is :melody))
        (where :time (bpm bpm-))
        (where :pitch (comp key- mode))
        play))

    (let* [lengths     (map (fn [_] NOTEVAL) melody)
           both        (fold-holds melody lengths)
           new-melody  (map first both)
           new-lengths (map second both)]
      (play-melody key- mode bpm- (into [] new-melody) new-lengths)))

  ;; Convert a list of notes and holds into a phrase structure for use
  ;; with Leipzig
  (defn i->phrase [melody]
    (let* [melody-with-rests (map (fn [x] (if (rest? x) x x)) melody)
           lengths     (map (fn [_] NOTEVAL) melody-with-rests)
           both        (fold-holds melody-with-rests lengths)
           new-melody  (map first both)
           new-lengths (map second both)]
      (phrase new-lengths new-melody)))

  ;; Given the parameters of a sonata-allegro composition, collect the parameters and play
  (defn play-sonata [key1- key2- mode- bpm- chords1 theme1 chords2 theme2 devchords development tr1 tr2]

    ;; Make a chord phrase given a list of roots and holds
    (defn make-chords [roots key- mode-]
      (let [fold (fold-holds  roots (map (fn [_] NOTEVAL) roots))]
        (->> (phrase (map second fold)
                     (into [] (map (fn [x]
                                     (-> chord/triad (chord/root x) (dissoc :v)))
                                   (map first fold))))
          (wherever :pitch, :pitch lower)
          (where :pitch (comp key- mode-))
          (where :part (is :default)))))

    (let* [m1   (->> (i->phrase theme1)      (where :pitch (comp key1- mode-)) (where :part (is :default)))
           m1'  (->> (i->phrase theme1)      (where :pitch (comp key2- mode-)) (where :part (is :default)))
           m2   (->> (i->phrase theme2)      (where :pitch (comp key1- mode-)) (where :part (is :default)))
           m2'  (->> (i->phrase theme2)      (where :pitch (comp key2- mode-)) (where :part (is :default)))
           dev' (->> (i->phrase development) (where :pitch (comp key2- mode-)) (where :part (is :default)))
           nchords1  (make-chords chords1 key1- mode-)
           nchords1' (make-chords chords1 key2- mode-)
           nchords2  (make-chords chords2 key1- mode-)
           nchords2' (make-chords chords2 key2- mode-)
           nchords3' (make-chords devchords key2- mode-)]
      (->>
        m1
        (then (with m1   nchords1))
        (then (with m2'  nchords2'))
        (then (with m1'  nchords1'))
        (then (with m2'  nchords2'))
        (then (with dev' nchords3'))
        (then (with m1   nchords1))
        (then (with m2   nchords2))
        (where :duration (bpm bpm-))
        (where :time (bpm bpm-))
        play)))

  (let [s1  (first s)
        s2  (second s)
        dev (second (rest s))]
    (play-sonata (get settings :key1) 
                 (get settings :key2) 
                 (get settings :mode) 
                 (get settings :bpm)
                 (inst-chords (first s1) (second s1))
                 (first s1)
                 (inst-chords (first s2) (second s2))
                 (first s2)
                 (inst-chords (first dev) (second dev))
                 (first dev)
                 nil nil)))

(defn resolve-key [k]
  (case k "A" A "B" B "C" C "D" D "E" E "F" F "G" G))

(defn resolve-mode [m]
  (case m "major" major "minor" minor))

(defn -main [& args]
  (let* [infile (first args)
         song (read-string (slurp infile))
         key1 (second args)
         key2 (second (rest args))
         mode (second (rest (rest args)))
         bpm  (second (rest (rest (rest args))))]
    (play-song song {:key1 (resolve-key key1) 
                     :key2 (resolve-key key2) 
                     :mode (resolve-mode mode) 
                     :bpm (read-string bpm)})))


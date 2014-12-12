(ns evol.midi
  (:use music-compojure
        music-compojure.notes
        music-compojure.generators))

(require '[evol.play :refer [inst-chords]])
(require '[evol.utils :refer :all])

(inst-chords (list 1 2 3 4) (list 1 HOLD 2 HOLD))

;; Write a list of holds and notes as a midi file to the given file location
(defn melody->midi [melody chords file]
  (def cmajor [-13 -12 -10 -8 -7 -5 -3 -1 0 2 4 5 7 9 11 12 14])
  (defn make-chord [root length]
    [(list root (+ root 3) (+ root 5)) length])

  (defn line->vec [line]
    (let* [melody-with-rests (map (fn [x] (if (rest? x) x x)) melody)
           lengths     (map (fn [_] NOTEVAL) melody-with-rests)
           both        (fold-holds melody-with-rests lengths)
           new-melody  (map first both)
           new-lengths (map second both)
           midi (map (fn [x] [(+ 60 (get cmajor (- (first x) MINNOTE)))
                              (/ 1 (* 1/4 (second x)))])
                     both)]
        (vec midi)))

    ;(create-midi-file (vec (map (fn [x] (make-chord (first x) (second x))) (line->vec (inst-chords melody chords)))) file))

 ; (create-midi-file (map (fn [x] (make-chord (first x) (second x))) (line->vec melody)) file))
  (create-midi-file [{:spacing-inverted true}
                     (_ [{:channel 1 :program 1} (line->vec melody)]
                        [{:channel 2 :program 2} (vec (map (fn [x] (make-chord (- (first x) 12) (second x))) (line->vec (inst-chords melody chords))))] _) 0]
                    file))

(map (fn [x] (+ x 1)) (vec (list 1 2)))

;; Convert a song structure of the form,
;;     [ [theme1 chords1] [theme2 chords2] [devel chords] ]
;; to a midi file at the given location
(defn song->midi [song file]
  (let [m (map first song)
        c (map second song)]
    (melody->midi
     (flatten (concat (first  m)
                      (second m)
                      (first  m)
                      (second m)
                      (second (rest m))
                      (first  m)
                      (second m)))
     (flatten (concat (first  c)
                      (second c)
                      (first  c)
                      (second c)
                      (second (rest c))
                      (first  c)
                      (second c)))
     file)))

(ns evol.midi
  (:gen-class)
  (:use music-compojure
        music-compojure.notes
        music-compojure.generators))

(require '[evol.utils :refer :all])

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

    (create-midi-file (line->vec melody) file))

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

(defn -main [& args] 
  (let* [infile (first args)
         outfile (second args)
         contents (read-string (slurp infile))]
    (song->midi contents outfile)))


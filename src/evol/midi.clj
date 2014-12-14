(ns evol.midi
  (:gen-class)
  (:use music-compojure
        music-compojure.notes
        music-compojure.generators))

(require '[evol.utils :refer :all])

;; Write a list of holds and notes as a midi file to the given file location
(defn melody->midi [melody chords file]
  (def cmajor [-13 -12 -10 -8 -7 -5 -3 -1 0 2 4 5 7 9 11 12 14])
  (defn make-chord [root]
    (list root (+ root 4) (+ root 7)))
  (defn lower [note] (- note 12))
  (defn midi-music [melody chords]
    [{:spacing-inverted true}
     (_ [ {:channel 1 :program 1} melody]
        [ {:channel 2 :program 1} chords] _) 0 ])
  (defn line->vec [line]
    (let* [melody-with-rests (map (fn [x] (if (rest? x) x x)) line)
           lengths     (map (fn [_] NOTEVAL) melody-with-rests)
           both        (fold-holds melody-with-rests lengths)
           new-melody  (map first both)
           new-lengths (map second both)
           midi (map (fn [x] [(+ 60 (get cmajor (- (first x) MINNOTE)))
                              (/ 1 (* 1/4 (second x)))])
                     both)]
      (vec midi)))

  ;; Create the midi file
  (create-midi-file 
    (midi-music

      ;; Make the melodic midi line
      (line->vec melody) 

      ;; Instantiate chords under the melody and make the chord midi line
      (vec (map (fn [x] [(make-chord (lower (first x))) (second x)]) 
                (line->vec (inst-chords melody chords))) ))
    file))

;; Convert a song data structure to a sonata-allegro form line
(defn sonata-allegro [song]
  (flatten (concat (first  song) (second song)
                   (first  song) (second song)
                   (second (rest song)) 
                   (first  song) (second song))))

;; Convert a song structure of the form,
;;     [ [theme1 chords1] [theme2 chords2] [devel chords] ]
;; to a midi file at the given location
(defn song->midi [song file]
  (let [melody  (map first song)
        chords (map second song)]
    (melody->midi (sonata-allegro melody) (sonata-allegro chords) file)))

(defn -main [& args] 
  (let* [infile   (first args)
         outfile  (second args)
         contents (read-string (slurp infile))]
    (song->midi contents outfile)))


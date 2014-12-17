(ns evol.fitness)

(require '[evol.utils :refer :all])

;; FITNESS
;; =======
;;
;; Phrase Fitness
;; --------------
;;
;; * Average intervallic movement relative to velocity
;; * Frequency of note neighbours
;; * Std. Dev and variance of velocity of notes
;;     * There should be more notes than rests on the downbeat
;;     * General hill shape
;; * Average patterning
;; * Interval shape
;; * Interval distribution
;; * Rhythm distribution
;;
;; Theme Fitness
;; -------------
;;
;; * Phrase fitness *plus*:
;;     * Start on tonic
;;     * End on tonic
;;
;; Transition Fitness
;; ------------------
;;
;; Given a start and end note, then
;; * Phrase fitness *plus*:
;;     * Start on start note
;;     * End on end note
;;

;; ***********
;;   HELPERS
;; ***********

;; Truncate a number to two decimal places (useful for printing)
(defn two-decimals [n]
  (/ (Math/floor (* 100 n)) 100))

;; Convert a list of note values into a list of intervals between notes
(defn difference [l]
  (if (empty? (rest l))
    nil
    (second (reduce (fn [prev curr]
                      (list curr (concat (second prev) (list (- curr (first prev))))))
                    (list (first l) nil)
                    l))))

;; Normalize a number to a signed 1
(defn normalize [x] (if (= 0 x) 0 (/ x (Math/abs x))))

;; Normalize a list of numbers to a list of signed 1s
(defn normalize-list [l]
  (map normalize l))

;; Sum a normalized list
(defn normal-sum [l]
  (reduce + 0 (normalize-list l)))

;; Return a pair representing the normalized sum of each half of a list
(defn normal-halves [l strlen]
  (let* [start (take (/ strlen 2) l)
         end   (drop (/ strlen 2) l)
         nrmls (map (fn [x] (normal-sum (difference x))) (list start end))]
    nrmls))

;; Absolute value
(defn abs [x] (Math/abs x))

;; Return the number of occurrences of each item in a list
(defn count-occurences [notes]
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} notes))

;; Return the frequency of occurrences of each note length
(defn get-length-frequencies [melody]
  (let [lengths (map count (get-notes melody))
        c (count lengths)
        occurences (sort (count-occurences lengths))
        length-domain (map first occurences)
        freqs (map (fn [x] [(first x) (two-decimals (double (/ (second x) c)))]) occurences)]
    freqs))

;; Return the Kullback-Leibler divergence of two distributions
(defn relative-entropy [distro1 distro2]
  (reduce + 0
          (map
            (fn [x] (* (first x) (Math/log (/ (first x) (second x)))))
            (zip distro1 distro2))))

(defn get-interval-frequencies [melody]
  (let [notes (map first (get-notes melody))
        c (count notes)
        intervals (map abs (difference notes))
        occurences (sort (count-occurences intervals))
        interval-domain (map first occurences)
        freqs (map (fn [x] [(first x)
                            (two-decimals (double (/ (second x) c)))])
                   occurences)]
    freqs))


(defn lists-of-size-4 [l]
  (partition 4 (flatten
                 (zip
                   (zip
                     (zip l (rest l))
                     (rest (rest l)))
                   (rest (rest (rest l)))))))


(defn num-same-sublists [l1 l2]
  (def compt (lists-of-size-4 l2))
  (defn num-same-sublists-single [l1']
    (count (filter true? (flatten (map (fn [x] (= l1' x)) compt)))))
  (reduce + 0 (map second (reduce (fn [acc x]
            (let [prev (get acc x 0)]
              (assoc acc x (max prev (num-same-sublists-single x)))))
          {}
          (lists-of-size-4 l1)))))

;; ***********
;;   FITNESS
;; ***********

;; Return 0 minus the number of repeating rhythmic patterns
;; of the given length are in the given melody
(defn fit-repeating-rhythm [melody length]
  (defn l [s len i limit m]
    (if (>= i limit)
      (- 0 (reduce + 0 (map second (filter (fn [x] (< 1 (second x))) m))))
      (let [sub (take len s)
            n (get m sub 0)]
        (recur (rest s) len (+ 1 i) limit (assoc m sub (+ n 1))))))
  (l (map count (get-notes melody)) length 0 (+ 1 (- (count melody) length)) {}))

;; Return 0 mnus the number of repeating notes of a given
;; length are in the given melody
(defn fit-repeating-notes [melody length]
  (defn l [s len i limit m ]
    (if (>= i limit)
      (- 0 (reduce + 0 (map second (filter (fn [x] (< 1 (second x))) m))))
      (let [sub (take len s)
            n (get m sub 0)]
        (recur (rest s) len (+ 1 i) limit (assoc m sub (+ n 1))))))
  (let [notes (map first (get-notes melody))]
    (l notes length 0 (+ 1 (- (count notes) length)) {})))

;; Return the sum of the intervallic movement in a melody.
;; Awards for having less intervallic jumps in a piece
(defn fit-melodic-intervals [melody]
  (let [notes (map first (get-notes melody))
        c     (count notes)]
    (if (= c 0)
      0
      (/ (reduce + 0 (map abs (difference notes))) c))))

;; Return the number of intervals that are larger than a given cutoff
;; Awards for not having intervals larger than some max
(defn fit-largest-interval [melody cutoff]
  (let [intervals (map abs (difference (map first (get-notes melody))))]
    (count (filter (fn [x] (> x cutoff)) intervals))))

;; Award for having a sloped first half, whether up or down
(defn fit-slope-first-half [melody]
  (let [n (map first (get-notes melody))
        c (count n)
        h (take (/ c 2) n)]
    (- (- (/ c 2) 4)
       (Math/abs (normal-sum (difference h))))))

;; Award for having a sloped second half, whether up or down
(defn fit-slope-second-half [melody]
  (let [n (map first (get-notes melody))
        c (count melody)
        h (drop (/ c 2) n)]
    (- (- (/ c 2) 1)
       (Math/abs (normal-sum (difference h)) )) ))

;; *************
;; THEME FITNESS
;; *************

;; 1) Start on tonic
;; Minimizing fitness score
(defn fit-start-on-tonic [melody]
  (Math/abs (first melody)))

;; 2) End on tonic
(defn fit-end-on-tonic [melody]
  (Math/abs (first (reverse melody))))

;; Return the number of holds that are on an on-beat.
;; Awards for having off-beat rests, assuming rests are present
(defn fit-on-beat-notes [melody]
  (let [{odds true evens false}
        (group-by odd?
                  (map first
                       (filter (fn [x] (hold? (second x)))
                               (map-indexed list melody))))]
    (count evens)))

;; Return 1 if the melody does not end in a (5 _), and 0 if it does.
;; Penalizes if not having the components of a perfect cadence
(defn fit-perfect-candence-end [melody]
  (Math/abs (- (Math/abs (second (reverse melody))) 5)))

;; Returns the deviation of the frequency of holds from a given
;; expectation
(defn fit-hold-ratio [melody rate]
  (let [{holds true others false} (group-by hold? melody)]
    (if (= (count holds) 0) 1
      (Math/abs (- rate (/ (count holds) (count melody)))))))

;; Returns the deviation of the frequency of rests from a given
;; expectation
(defn fit-rest-ratio [melody rate]
  (let [{rests true others false} (group-by rest? melody)]
    (if (= (count rests) 0) 1
      (Math/abs (- rate (/ (count rests) (count melody)))))))

;; Awards for having an up-down hillshape based on the intervallic movement
;; in each half of a melody
(defn fit-hill-shape [melody]
  (let [c (count melody)
        diff (normal-halves melody c)
        sub  (+ (first diff) (second diff))]
    (if (= sub 0) 0
      (/ sub (normalize sub)))))

(defn fit-heaviest [bar]
  (let* [n (get-notes bar)
         m (reduce (fn [m x] (assoc m
                                    (first x)
                                    (+ (count x) (get m (first x) 0))))
                   {} n)
         heaviest  (second (first (reverse (sort-by second m))))]
    (- heaviest (get m 0 2))))

;; Award for having a higher precense of the tonic in the last measure
(defn fit-tonic-heaviest-last [melody]
  (let [b (last-bar melody)]
    (fit-heaviest b)))

;; Award for having a higher precense of the tonic in the first measure
(defn fit-tonic-heaviest-first [melody]
  (let [b (first-bar melody)]
    (fit-heaviest b)))

;; Return the number of holds that are present during a measure downbeat
(defn fit-note-on-downbeats [melody]
  (let [downbeats (map first (partition 8 melody))]
    (count (filter (fn [x] (hold? x)) downbeats))))

;; Returns the Kullback-Leibler divergence of the desired distribution
;; of intervallic movement and the observed distribution
(defn fit-interval-distribution [melody baseline]
  (let [notes (map first (get-notes melody))
        c (count notes)
        intervals (map abs (difference notes))
        occurences (sort (count-occurences intervals))
        interval-domain (map first occurences)
        freqs (map (fn [x] [(first x) (/ (second x) c)]) occurences)]
    (if (not (= (map first baseline) interval-domain))
      10
      (relative-entropy (map second (sort baseline)) (map second (sort freqs))))))

;; Returns the Kullback-Leibler divergence of the desired distribution
;; of hold lengths and the observed distribution
(defn fit-length-distribution [melody baseline]
  (let [lengths (map count (get-notes melody))
        c (count lengths)
        occurences (sort (count-occurences lengths))
        length-domain (map first occurences)
        freqs (map (fn [x] [(first x) (/ (second x) c)]) occurences)]
    (if (not (= (map first baseline) length-domain))
      10
      (relative-entropy (map second (sort baseline)) (map second (sort freqs))))))

(defn fit-repeating-note-appearances [melody1 melody2]
  (let [notes1 (map first (get-notes melody1))
        notes2 (map first (get-notes melody2))]
    (- 0 (num-same-sublists notes1 notes2))))

(defn fit-repeating-rhythm-appearances [melody1 melody2]
  (let [notes1 (map count (get-notes melody1))
        notes2 (map count (get-notes melody2))]
    (- 0 (num-same-sublists notes1 notes2))))

(defn cooperative-fitness [theme1 theme2]
  (fn [melody]
    (+ (* 1/2 (fit-repeating-note-appearances melody theme1))
       (* 1/2 (fit-repeating-rhythm-appearances melody theme1))
       (* 1/2 (fit-repeating-note-appearances melody theme2))
       (* 1/2 (fit-repeating-rhythm-appearances melody theme2)))))

;; ***
;; API
;; ***

(defn fitness-phrase [melody settings]
  (+ (* 50  (fit-start-on-tonic melody))
     (* 50  (fit-end-on-tonic melody))
     (* 200 (fit-note-on-downbeats melody))
     (* 800 (fit-interval-distribution melody (get settings "interval-distribution")))
     (* 800 (fit-length-distribution melody (get settings "duration-distribution")))
     (* 150 (fit-on-beat-notes melody))))

;; Return the fitness for a theme
(defn fitness-theme [melody settings]
  (+ (fitness-phrase melody settings)
     (* 10  (fit-repeating-rhythm melody 5))
     (* 5   (fit-repeating-rhythm melody 3))
     (* 10  (fit-repeating-notes melody 5))
     (* 5   (fit-repeating-notes melody 3))
     (* 100 (fit-hill-shape melody))))

(defn fitness-development [melody theme1 theme2 settings]
  (+ (* 2 (fitness-phrase melody settings))
     (reduce + 0 (map (fn [x] (* 100 (fit-hill-shape x))) (partition PHRASELEN melody)))
     ((cooperative-fitness theme1 theme2) melody)))

;; Return the fitness for a chord rhythm
(defn fitness-chord [melody settings]
  (+ (* 50  (fit-start-on-tonic melody))
     (* 50  (fit-end-on-tonic melody))
     (* 200 (fit-note-on-downbeats melody))
     (* 10  (fit-repeating-rhythm melody 5))
     (* 5   (fit-repeating-rhythm melody 3))
     (* 800 (fit-length-distribution melody (get settings "chord-dur-distribution")))
     (* 20  (fit-on-beat-notes melody))))

;; Print the fitness report for a melody
(defn print-fitness-info [typ melody]
  (print "[FITNESS]  ")
  (println (melody->str melody))
  (print "  Start on tonic         : ")
  (println (fit-start-on-tonic melody))
  (print "  End on tonic           : ")
  (println (fit-end-on-tonic melody))
  (print "  Hill shape             : ")
  (println (fit-hill-shape melody))
  (print "  Rest Ratio             : ")
  (println (fit-rest-ratio melody 0.4))
  (print "  Note on beat           : ")
  (println (fit-on-beat-notes melody))
  (print "  Melodic interval       : ")
  (println (fit-melodic-intervals melody))
  (print "  Fit repeating patter 5 : ")
  (println (fit-repeating-notes melody 5))
  (print "  Fit repeating rhythm 5 : ")
  (println (fit-repeating-rhythm melody 5))
  (print "  Fit repeating patter 3 : ")
  (println (fit-repeating-notes melody 3))
  (print "  Fit repeating rhythm 3 : ")
  (println (fit-repeating-rhythm melody 3))
  (print "  Fit onbeat notes       : ")
  (println (fit-on-beat-notes melody))
  (print "  Fit notes on downbeat  : ")
  (println (fit-note-on-downbeats melody))
  (print "  Interval distro        : ")
  (println (fit-interval-distribution melody [[0 0.05] [1 0.48] [2 0.28] [3 0.05] [4 0.06] [5 0.08]]))
  (print "  Length distro          : ")
  (println (fit-length-distribution melody   [[1 0.6] [2 0.20] [3 0.1] [4 0.1]])))

;; Return the fitness for a given type of individual
(defn fitness [type- themes settings]
  (fn [melody]
    (cond
      (= type- 'theme)       (fitness-theme melody settings)
      (= type- 'chord)       (fitness-chord melody settings)
      (= type- 'development) (fitness-development melody
                                                  (first themes)
                                                  (second themes)
                                                  settings))))


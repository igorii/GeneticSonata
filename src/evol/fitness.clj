(ns evol.fitness)

(require '[evol.utils :refer :all])

;; SPECIES
;; =======
;;
;; Song Species
;; ------------
;;
;; Five species cooperative coevolution for sonata-allegro
;;     S1 -> Theme1 species (phrase)
;;     S2 -> Theme2 species (phrase)
;;     S3 -> Transition (I V species) (phrase)
;;     S4 -> Transition (V I species) (phrase)
;;     S5 -> Development species (phrases)
;;
;;
;; Theme Species
;; -------------
;;
;; Single population of phrases
;;
;;
;; Transition Species
;; ------------------
;;
;; Single population of phrases
;;
;;
;; Development species
;; -------------------
;;
;; Two population single evolution
;;     P1 -> Population of indices into phrase pop
;;     P2 -> Population of phrases
;;
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
;;
;; Theme Fitness
;; -------------
;;
;;     * Phrase fitness *plus*:
;;     * Start on tonic
;;     * End on tonic
;;
;; Transition Fitness
;; ------------------
;;
;; Given a start and end note, then
;; * Phrase fitness *plus*:
;;   * Start on start note
;;   * End on end note
;;   * Slow down over time
;;
;; Development Fitness
;; -------------------
;;
;; Song Fitness
;; ------------
;;

(defn fit-repeating-rhythm [melody length]
  (defn l [s len i limit m]
    (if (>= i limit)
      (- 0 (reduce + 0 (map second (filter (fn [x] (< 1 (second x))) m))))
      (let [sub (take len s)
            n (get m sub 0)]
        (recur (rest s) len (+ 1 i) limit (assoc m sub (+ n 1))))))
  (l (map count (get-notes melody)) length 0 (+ 1 (- (count melody) length)) {}))


(defn fit-repeating-notes [melody length]
  (defn l [s len i limit m ]
    (if (>= i limit)
      (- 0 (reduce + 0 (map second (filter (fn [x] (< 1 (second x))) m))))
      (let [sub (take len s)
            n (get m sub 0)]
        (recur (rest s) len (+ 1 i) limit (assoc m sub (+ n 1))))))
  (let [notes (map first (get-notes melody))]
    (l notes length 0 (+ 1 (- (count notes) length)) {})))

(defn difference [l]
  (if (empty? (rest l))
    nil
    (second (reduce (fn [prev curr]
              (list curr (concat (second prev) (list (- curr (first prev))))))
            (list (first l) nil)
            l))))

(defn normalize [x] (if (= 0 x) 0 (/ x (Math/abs x))))

(defn normalize-list [l]
  (map normalize l))

(defn normal-sum [l]
  (reduce + 0 (normalize-list l)))

(defn normal-halves [l strlen]
  (let* [start (take (/ strlen 2) l)
         end   (drop (/ strlen 2) l)
         nrmls (map (fn [x] (normal-sum (difference x))) (list start end))]
    nrmls))

(defn abs [x] (Math/abs x))

(defn fit-melodic-intervals [melody]
  (let [notes (map first (get-notes melody))
        c     (count notes)]
    (if (= c 0)
      0
      (/ (reduce + 0 (map abs (difference notes))) c))))

(defn fit-largest-interval [melody cutoff]
  (let [intervals (map abs (difference (map first (get-notes melody))))]
     (count (filter (fn [x] (> x cutoff)) intervals))))

(difference (list 2 4 6 8))
(fit-melodic-intervals (list 0 2 4 6))
(map abs (difference
 (map first (get-notes
             (list 0 2 -9 3 -9 4 -9 4 5 8 9 8 6 8 8 -9 5 5 -9 4 -9 6 -9 8 -9 8 1 1 -9 0 -9 5 0)))))

(defn fit-slope-first-half [melody]
  (let [n (map first (get-notes melody))
        c (count n)
        h (take (/ c 2) n)]
    (- (- (/ c 2) 4)
       (Math/abs (normal-sum (difference h)) )) ))

(defn fit-slope-second-half [melody]
  (let [n (map first (get-notes melody))
        c (count melody)
        h (drop (/ c 2) n)]
    (- (- (/ c 2) 1)
       (Math/abs (normal-sum (difference h)) )) ))

(defn count-occurences [notes]
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} notes))

;; ******
;; Themes
;; ******

;; 1) Start on tonic
;; Minimizing fitness score
;; max = , min =
(defn fit-start-on-tonic [melody]
  (Math/abs (first melody)))

;; 2) End on tonic
;; max = , min =
(defn fit-end-on-tonic [melody]
  (Math/abs (first (reverse melody))))

(defn fit-on-beat-notes [melody]
  (let [{odds true evens false} (group-by odd?
                                          (map first
                                               (filter (fn [x] (= HOLD (second x)))
                                                       (map-indexed list melody))))]
          ;odd-count  (count odds)
          ;even-count (count evens)]
    (count evens)))

;; max =, min =
(defn fit-perfect-candence-end [melody]
  (Math/abs (- (Math/abs (second (reverse melody))) 5)))

;; max = , min =
(defn fit-half-candence-middle [melody]
  (let* [rm2 (reverse (drop (/ (count melody) 2) melody))]
    (+
      (Math/abs (- (Math/abs (first rm2)) 5))
      (Math/abs (- (Math/abs (second rm2)) 0)))))

;; max = , min =
(defn fit-hold-ratio [melody]
  (let [{holds true others false} (group-by hold? melody)]
    (if (= (count holds) 0) 1
      (Math/abs (- 0.3 (/ (count holds) (count melody)))))))

(defn fit-rest-ratio [melody]
  (let [{rests true others false} (group-by rest? melody)]
    (if (= (count rests) 0) 1
      (Math/abs (- 0.3 (/ (count rests) (count melody)))))))

;; max = , min =
(defn fit-hill-shape [melody]
  (let [c (count melody)
        diff (normal-halves melody c)
        sub  (+ (first diff) (second diff))]
    (if (= sub 0) 0
      (/ sub (normalize sub)))))

(defn fit-tonic-heaviest-last [melody]
  (let* [b (last-bar melody)
         n (get-notes b)
         m (reduce (fn [m x] (assoc m (first x) (+ (count x) (get m (first x) 0)))) {} n)
         heaviest  (second (first (reverse (sort-by second m))))]
        (- heaviest (get m 0 2))))

(defn fit-tonic-heaviest-first [melody]
  (let* [b (first-bar melody)
         n (get-notes b)
         m (reduce (fn [m x] (assoc m (first x) (+ (count x) (get m (first x) 0)))) {} n)
         heaviest  (second (first (reverse (sort-by second m))))]
        (- heaviest (get m 0 2))))

(defn fit-note-on-downbeats [melody]
  (let [downbeats (map first (partition 8 melody))]
    (count (filter (fn [x] (hold? x)) downbeats))))

(defn relative-entropy [distro1 distro2]
  (reduce + 0 (map (fn [x] (* (first x) (Math/log (/ (first x) (second x))))) (zip distro1 distro2))))

(relative-entropy (list 0.1 0.2 0.7) (list 0.))

(defn get-interval-frequencies [melody]
  (let [notes (map first (get-notes melody))
        c (count notes)
        intervals (map abs (difference notes))
        occurences (sort (count-occurences intervals))
        interval-domain (map first occurences)
        freqs (map (fn [x] [(first x) (two-decimals (double (/ (second x) c)))]) occurences)]
    freqs))

(defn get-length-frequencies [melody]
  (let [lengths (map count (get-notes melody))
        c (count lengths)
        occurences (sort (count-occurences lengths))
        length-domain (map first occurences)
        freqs (map (fn [x] [(first x) (two-decimals (double (/ (second x) c)))]) occurences)]
    freqs))


(defn fit-interval-distribution [melody baseline]
  (let [notes (map first (get-notes melody))
        c (count notes)
        intervals (map abs (difference notes))
        occurences (sort (count-occurences intervals))
        interval-domain (map first occurences)
        freqs (map (fn [x] [(first x) (/ (second x) c)]) occurences)
        ]
    (if (not (= (map first baseline) interval-domain))
      10
      (relative-entropy (map second (sort baseline)) (map second (sort freqs))))
  ))


 (defn fit-length-distribution [melody baseline]
  (let [lengths (map count (get-notes melody))
        c (count lengths)
        occurences (sort (count-occurences lengths))
        length-domain (map first occurences)
        freqs (map (fn [x] [(first x) (/ (second x) c)]) occurences)
        ]
    (if (not (= (map first baseline) length-domain))
      10
      (relative-entropy (map second (sort baseline)) (map second (sort freqs))))
  ))


 (get-length-frequencies (list 1 HOLD 2 HOLD HOLD 3 4 HOLD))

  (count-occurences (map abs (difference (map first (get-notes (list 0 HOLD 1 HOLD HOLD 3 HOLD HOLD HOLD 6 HOLD HOLD HOLD HOLD))))))
(fit-length-distribution (list 0 0 1 HOLD HOLD 3 HOLD HOLD HOLD 6 HOLD HOLD HOLD HOLD)
                           [[0 1/4] [1 1/4] [2 1/4] [3 1/4]]
                           )

(= (list 1 3) (list 1 2))

;(map first (partition 2 (list 1 2 1 2 1 2 1 2 1 2)))

(sort (count-occurences (list 1 2 1 1 0 0 2 3 1)))

;; ******
;; Phrase
;; ******

;; ***
;; API
;; ***

(defn fitness-theme [melody key-]
  (+ (* 50 (fit-start-on-tonic         melody))
     (* 50 (fit-end-on-tonic         melody))
     (* 200 (fit-note-on-downbeats     melody))
     (* 100 (fit-hill-shape           melody))
     ;(* 5 (fit-melodic-intervals    melody))
    ; (* 100 (fit-slope-first-half       melody))
     ;(* 3 (fit-largest-interval melody 6))
     ;(* 1 (fit-tonic-heaviest-last       melody))
     ;(* 1 (fit-tonic-heaviest-first       melody))
     ;(* 1 (fit-tonic-heaviest-first       melody))
     (* 10 (fit-repeating-rhythm melody 5))
     (* 5 (fit-repeating-rhythm melody  3))
     (* 10 (fit-repeating-notes melody  5))
     (* 5 (fit-repeating-notes melody   3))
   ;  (* 100 (fit-slope-second-half      melody))
     (* 800 (fit-interval-distribution melody [[0 0.05] [1 0.48] [2 0.28] [3 0.05] [4 0.06] [5 0.08]]))
     (* 800 (fit-length-distribution melody   [[1 0.6] [2 0.20] [3 0.1] [4 0.1]]))
     (* 20 (fit-on-beat-notes        melody))))
     ;(* 5 (fit-rest-ratio           melody))
     ;(* 200 (fit-hold-ratio           melody))))
     ;(* 1 (fit-perfect-candence-end melody))))
     ;(* 1/7 (fit-half-candence-middle melody))))

(defn fitness-chord [melody key-]
  (+ (* 50 (fit-start-on-tonic         melody))
     (* 50 (fit-end-on-tonic         melody))
     (* 200 (fit-note-on-downbeats     melody))
     (* 100 (fit-hill-shape           melody))
     ;(* 5 (fit-melodic-intervals    melody))
    ; (* 100 (fit-slope-first-half       melody))
     ;(* 3 (fit-largest-interval melody 6))
     ;(* 1 (fit-tonic-heaviest-last       melody))
     ;(* 1 (fit-tonic-heaviest-first       melody))
     ;(* 1 (fit-tonic-heaviest-first       melody))
     (* 10 (fit-repeating-rhythm melody 5))
     (* 5 (fit-repeating-rhythm melody  3))
     (* 10 (fit-repeating-notes melody  5))
     (* 5 (fit-repeating-notes melody   3))
   ;  (* 100 (fit-slope-second-half      melody))
     (* 800 (fit-interval-distribution melody [[0 0.05] [1 0.48] [2 0.28] [3 0.05] [4 0.06] [5 0.08]]))
     (* 800 (fit-length-distribution melody   [[1 0.05] [2 0.05] [3 0.05] [4 0.85]]))
     (* 20 (fit-on-beat-notes        melody))))
     ;(* 5 (fit-rest-ratio           melody))
     ;(* 200 (fit-hold-ratio           melody))))
     ;(* 1 (fit-perfect-candence-end melody))))
     ;(* 1/7 (fit-half-candence-middle melody))))

(defn two-decimals [n]
     (/ (Math/floor (* 100 n)) 100))

(defn print-fitness-info [melody]
    (println (melody->str melody))
    (print "  Start on tonic       : ")
    (println (fit-start-on-tonic melody))
    (print "  End on tonic         : ")
    (println (fit-end-on-tonic melody))
    (print "  Hill shape           : ")
    (println (fit-hill-shape           melody))
    (print "  Rest Ratio           : ")
    (println (fit-rest-ratio melody))
    (print "  Note on beat         : ")
    (println (fit-on-beat-notes melody))
    (print "  Melodic interval     : ")
    (println (fit-melodic-intervals melody))
    (print "  Fit repeating patter : ")
    (println (fit-repeating-notes melody 6))
    (print "  Fit repeating rhythm : ")
    (println (fit-repeating-rhythm melody 5))
    (print "  Interval distro      : ")
    (println (fit-interval-distribution melody [[0 0.05] [1 0.48] [2 0.28] [3 0.05] [4 0.06] [5 0.08]]))
    (print "  Length distro        : ")
    (println (fit-length-distribution melody   [[1 0.6] [2 0.20] [3 0.1] [4 0.1]]))

  )

(defn fitness [type- key-]
  (fn [melody]
   ; (println (get-notes melody))
    (cond
      (= type- 'theme)       (fitness-theme melody key-)
      (= type- 'chord)       (fitness-chord melody key-)
      (= type- 'development) (fitness-theme melody key-))))


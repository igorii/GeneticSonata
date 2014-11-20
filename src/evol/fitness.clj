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

(defn fit-repeating-pattern [melody length]
  (defn l [s len i limit m]
    (if (>= i limit)
      (- 100 (reduce + 0 (map second (filter (fn [x] (< 1 (second x))) m))))
      (let [sub (take len s)
            n (get m sub 0)]
        (recur (rest s) len (+ 1 i) limit (assoc m sub (+ n 1))))))
  (l (map count (get-notes melody)) length 0 (+ 1 (- (count melody) length)) {}))


(fit-repeating-pattern (list HOLD HOLD 1 HOLD HOLD 1 HOLD HOLD HOLD 1 HOLD 1 HOLD 1 HOLD HOLD 1 HOLD HOLD HOLD 1 HOLD 1 HOLD 1 HOLD) 6)


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

(difference (list 2 4 6 8))
(fit-melodic-intervals (list 0 2 4 6))
(map abs (difference
 (map first (get-notes
             (list 0 2 -9 3 -9 4 -9 4 5 8 8 6 8 8 -9 5 5 -9 4 -9 6 -9 8 -9 8 1 1 -9 0 -9 5 0)))))



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

(difference (list 1 HOLD 2 HOLD 3 HOLD 6 HOLD))

(def x (map first (get-notes (list 1 HOLD 2 HOLD 3 HOLD 6 HOLD))))
(def os (count-occurences (difference x)))

(map (fn [x] (println x)) os)

(def g 0)

(os 1)

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
(defn fit-rest-ratio [melody]
  (let [{rests true others false} (group-by hold? melody)]
    (if (= (count rests) 0) 1
      (Math/abs (- 0.4 (/ (count rests) (count melody)))))))

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


(fit-tonic-heaviest-last (list 0 HOLD 2 HOLD 4 5 6 HOLD))

;; ******
;; Phrase
;; ******

;; ***
;; API
;; ***

(defn fitness-theme [melody key-]
  (+ (* 5 (fit-start-on-tonic         melody))
     (* 5(fit-end-on-tonic         melody))
;     (* 1/7 (fit-hill-shape           melody))))
     (* 5 (fit-melodic-intervals    melody))
     (* 3 (fit-slope-first-half       melody))
     (* 1 (fit-tonic-heaviest-last       melody))
     (* 1 (fit-tonic-heaviest-first       melody))
     ;(* 1 (fit-repeating-pattern melody 5))
     (* 3 (fit-slope-second-half      melody))
     (* 1 (fit-on-beat-notes        melody))
     (* 5 (fit-rest-ratio           melody))
     (* 1 (fit-perfect-candence-end melody))))
     ;(* 1/7 (fit-half-candence-middle melody))))

(defn fitness-development [melody key-]
   (+
;     (* 1/7 (fit-hill-shape           melody))))
     (* 8 (fit-melodic-intervals    melody))
     (* 3 (fit-slope-first-half       melody))
     ;(* 2 (fit-tonic-heaviest-last       melody))
     ;(* 2 (fit-tonic-heaviest-first       melody))
    ; (* 1 (fit-repeating-pattern melody 3))
     (* 3 (fit-slope-second-half      melody))
     (* 1 (fit-on-beat-notes        melody))
     (* 5 (fit-rest-ratio           melody))))

     ;(* 1/7 (fit-half-candence-middle melody))

(defn fitness [type- key-]
  (fn [melody]
   ; (println (get-notes melody))
    (cond
      (= type- 'theme)       (fitness-theme melody key-)
      (= type- 'development) (fitness-development melody key-))))


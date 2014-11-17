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
;; * There should be more notes than rests on the downbeat
;; * General hill shape
;; * Average patterning
;; * Interval shape
;;
;; Theme Fitness
;; -------------
;;
;; * Phrase fitness *plus*:
;;   * Start on tonic
;;   * End on tonic
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


(defn difference [l]
  (if (empty? (rest l))
    (list 0)
    (reduce (fn [prev curr]
              (list curr (concat (second prev) (list (- curr (first prev))))))
            (list (first l) nil)
            l)))




;; ******
;; Themes
;; ******

;; 1) Start on tonic
;; Minimizing fitness score
(defn fit-start-on-tonic [melody]
  (Math/abs (first melody)))

;; 2) End on tonic
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

(defn fit-perfect-candence-end [melody]
  (Math/abs (- (Math/abs (second (reverse melody))) 5)))

(defn fit-half-candence-middle [melody]
  (let* [rm2 (reverse (drop (/ (count melody) 2) melody))]
    (+
      (Math/abs (- (Math/abs (first rm2)) 5))
      (Math/abs (- (Math/abs (second rm2)) 0)))))

(defn fit-rest-ratio [melody]
  (let [{rests true others false} (group-by ishold? melody)]
    (if (= rests 0) 0.6)
    (Math/abs (- 0.4 (/ (count others) (count rests))))))

;; ******
;; Phrase
;; ******


;; ***
;; API
;; ***

(defn fitness-theme [melody key-]
  (+ (* 2 (fit-start-on-tonic melody))
     (* 2 (fit-end-on-tonic   melody))
     (* 1 (fit-on-beat-notes  melody))
     (* 3 (fit-rest-ratio     melody))
     (* 2 (fit-perfect-candence-end melody))
     (* 2 (fit-half-candence-middle melody))))

(defn fitness [type- key-]
  (fn [melody]
    (cond
      (= type- 'theme) (fitness-theme melody key-))))


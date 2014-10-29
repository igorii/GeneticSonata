(ns evol.fitness)

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

;; ******
;; Phrase
;; ******


;; ***
;; API
;; ***

(defn fitness-theme [melody key-]
  (let [a (fit-start-on-tonic melody)
        b (fit-end-on-tonic   melody)]
    (+ a b)))

(defn fitness [type- key-]
  (fn [melody]
    (cond
      (= type- 'theme) (fitness-theme melody key-))))


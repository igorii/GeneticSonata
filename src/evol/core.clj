(ns evol.core
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        overtone.inst.sampled-piano
        overtone.inst.synth))

(require '[overtone.live :as overtone]
         '[leipzig.chord :as chord]
         '[leipzig.melody :refer [bpm is phrase then times where with]])

(require '[evol.fitness   :as fitness]
         '[evol.crossover :as crossover]
         '[evol.play      :as play]
         '[evol.selection :as selection]
         '[evol.mutation  :as mutation]
         '[evol.utils :refer :all])

;; Initialize a population of a given size
(defn init-population [size hold-rate rest-rate length domain]

  ;; Initialize one single line of music (one individual)
  (defn init-one [hold-rate rest-rate length domain]
    (map (fn [_]
           (let [diceroll (rand)]
             (if (> hold-rate diceroll) HOLD
               (if (> (+ hold-rate rest-rate) diceroll) REST
                 (from-domain domain))))) (range 0 length)))

  (map (fn [_] (init-one hold-rate rest-rate length domain)) (range 0 size)))

;; Given an population marked by fitness, create a new generation
;; using tournament selection of a given size
(defn create-next-gen [fpop psize tsize strlen mutation%]
  (apply concat
         ;; Use psize/2 tournaments creating 2 children each
         (map (fn [_]
                (let [candidates (apply crossover/one-point
                                        (concat (map second (selection/tournament fpop tsize))
                                                (list (rand-int (* 8 4)))))]

                  ;; Roll a dice and apply mutation if appropriate
                  (if (chance mutation%)
                    (list ((mutation/random) (first  candidates) strlen)
                          ((mutation/random) (second candidates) strlen))
                    candidates)))
              (range 0 (/ psize 2)))))


;; Given a measure population
(defn refine-measure-pop [init-pop popsize]
  (defn l [iter oldpop strlen]
    (let* [fits       (map (fitness/fitness 'development) oldpop)
           fpop       (map list fits oldpop)]
      (if (= 0 iter)
        oldpop
        (recur (- iter 1) (create-next-gen fpop popsize 4 strlen 0.9) strlen))))
  (l 10 init-pop 8))

;; Create a development list of notes and holds from a population of possible measures
(defn development-from-pop [population dev-length]
  (map (fn [x] (first (shuffle population))) (range 0 dev-length)))

(def last-song (ref nil))

;; Main entry point
(defn -main []
  (defn l [typ iter oldpop strlen popsize tourny-size mutation-rate fitness-data]
    (let* [fits (map (fitness/fitness typ fitness-data) oldpop)
           fpop (map list fits oldpop)
           best (min1 fpop)]
      (print iter) (print " ") (println [(first best)])
      (if (= 0 iter)
        (into [] (second best))
        (recur typ
               (- iter 1)
               (cons (second best) (create-next-gen fpop popsize tourny-size strlen mutation-rate))
               strlen
               popsize
               tourny-size
               mutation-rate
               fitness-data))))

  (let* [popsize       100
         iters         200
         hold-rate     0.4
         rest-rate     0
         tourny-size   4
         mutation-rate 0.3
         theme1-pop  (init-population popsize hold-rate rest-rate PHRASELEN NOTERANGE)
         chord1-pop  (init-population popsize hold-rate rest-rate PHRASELEN CHORDRANGE)
         theme2-pop  (init-population popsize hold-rate rest-rate PHRASELEN NOTERANGE)
         devel-pop   (init-population popsize hold-rate rest-rate (* 2 PHRASELEN) NOTERANGE)
         theme1      (l 'theme iters theme1-pop PHRASELEN popsize tourny-size mutation-rate nil)
         theme2      (l 'theme iters theme2-pop PHRASELEN popsize tourny-size mutation-rate nil)
         chords      (l 'chord iters chord1-pop PHRASELEN popsize tourny-size mutation-rate nil)
         development (l 'development (* 2 iters) devel-pop (* 2 PHRASELEN) popsize tourny-size mutation-rate [theme1 theme2])
         ;init-measure-pop (concat (list (first-bar  theme1))
         ;                         (list (last-bar   theme1))
         ;                         (list (first-bar  theme2))
         ;                         (list (last-bar   theme2))
         ;                         (init-population 30 0.4 0 8 NOTERANGE))
         ;dev-measure-pop (refine-measure-pop init-measure-pop 34)
         ;development     (development-from-pop dev-measure-pop 8)
         ]
   ; (println development)
    (println theme1)
    (println theme2)
    (dosync (ref-set last-song [[theme1 chords] [theme2 chords] [(flatten development) (flatten (repeat 2 chords))]]))
    (spit "event3.log"
          (str "\r\n\r\n"
               (str (deref last-song))) :append true)
    (recur)))
   ; (play/play-song (deref last-song))))

(play/play-song (deref last-song))

(-main)
(stop)

(ns evol.core (:gen-class))

(require '[evol.fitness   :as fitness]
         '[evol.crossover :as crossover]
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

;; Write a song to the given file
(defn song->file [file theme1 theme2 development chords]
    (spit file (str [[theme1 chords] 
                        [theme2 chords] 
                        [(into [] (flatten development)) 
                         (into [] (flatten (repeat 2 chords)))]]) 
          :append true))

;; Main entry point
(defn -main [& args]
  (defn l [typ iter oldpop strlen popsize tourny-size mutation-rate fitness-data]

    ;; Calculate fitness and find the best
    (let* [fits (map (fitness/fitness typ fitness-data) oldpop)
           fpop (map list fits oldpop)
           best (min1 fpop)]

      ;; Print progress info
      (do (print iter) 
          (print " ") 
          (println best))

      ;; If the last iteration, stop and return
      (if (= 0 iter)
        (do (println best)
            (into [] (second best)))

        ;; Loop until out of iterations
        (recur typ
               (- iter 1)
               (cons (second best) (create-next-gen fpop popsize tourny-size strlen mutation-rate))
               strlen
               popsize
               tourny-size
               mutation-rate
               fitness-data))))

  ;; GA parameters
  (let* [outfile       (first args)
         popsize       500
         iters         500
         hold-rate     0.4
         rest-rate     0
         tourny-size   4
         mutation-rate 0.3
         phraselen (* 8 4)

         ;; Run the GA for each sonata component
         theme1-pop  (init-population popsize hold-rate rest-rate phraselen NOTERANGE)
         chord1-pop  (init-population popsize hold-rate rest-rate phraselen CHORDRANGE)
         theme2-pop  (init-population popsize hold-rate rest-rate phraselen NOTERANGE)
         devel-pop   (init-population popsize hold-rate rest-rate (* 2 phraselen) NOTERANGE)
         theme1      (l 'theme iters theme1-pop phraselen popsize tourny-size mutation-rate nil)
         theme2      (l 'theme iters theme2-pop phraselen popsize tourny-size mutation-rate nil)
         chords      (l 'chord iters chord1-pop phraselen popsize tourny-size mutation-rate nil)
         development (l 'development (* 2 iters) devel-pop (* 2 phraselen) popsize tourny-size mutation-rate [theme1 theme2])
         ]

    ;; Print the song components to the specified file
    (song->file outfile theme1 theme2 development chords)))


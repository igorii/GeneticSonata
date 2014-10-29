(ns evol.utils)

(defn max1 [l]
  (reduce (fn [best x] (if (< (first x) (first best)) x best)) (first l) l))

(def HOLD -8)
(defn ishold? [x] (= x HOLD))

(defn get-notes [i]
  (defn inner [i acc]
    (if (empty? i) 
      acc
      (if (not (= HOLD (second i)))
        (recur (rest i) (concat acc (list (list (first i)))))
        (let [note (concat (list (first i)) (take-while ishold? (rest i)))
              next (drop-while ishold? (rest i))]
          (recur next (concat acc (list note)))))))
  (inner i nil))

;; Plays a single note
(defmethod play-note :default [{midi :pitch}] (sampled-piano midi))

;; Plays a list of notes for the given durations
(defn play-melody [key- mode bpm- pitches durations]
  (->> (phrase durations pitches)
    (where :part (is :melody))
    (where :time (bpm bpm-))
    (where :pitch (comp key- mode))
    play))

;; Return a single element renadomly from the domain
(defn from-domain [d] (d (rand-int (- (.length d) 1))))

;; Define fold holds
(defn fold-holds [melody lengths]
  (defn f [acc melody lengths]
    (if (empty? melody) acc
      (if (= (first melody) HOLD)
        (if (empty? acc)
          (f (cons (list (first melody) (first lengths)) acc) (rest melody) (rest lengths))
          (f (cons (list (first (first acc)) (+ 1/2 (second (first acc)))) (rest acc))
             (rest melody) (rest lengths)))
        (f (cons (list (first melody) (first lengths)) acc) (rest melody) (rest lengths)))))
  (f '() melody lengths))

;; Play a list of notes (currently each note is interpreted as an eigth note)
(defn play-one [key- mode bpm- melody]
  (let* [lengths (map (fn [_] 1/2) melody)
         both    (fold-holds melody lengths)
         new-melody (map first both)
         new-lengths (map second both)]
    (println both)
    (println new-melody)
    (println new-lengths)
    (play-melody key- mode bpm- (into [] new-melody) new-lengths)))

(ns evol.crossover)

;; Perform one-point crossover on two parents
(defn one-point [parent1 parent2 cross-point]

  (defn get-child [op cross-point p1 p2]
    (defn l [p1 p2 curr acc]
      (if (or (empty? p1) (empty? p2))
        acc
        (let [n (if (op curr cross-point) (first p1) (first p2))]
          (l (rest p1) (rest p2) (+ 1 curr) (concat acc (list n))))))
    (l parent1 parent2 0 '()))

  (list (get-child <= cross-point parent1 parent2)   ; Create the two children
        (get-child >  cross-point parent1 parent2)))

(ns evol.mutation)

(require '[evol.utils :refer [HOLD ishold? get-notes]])

(defn two-points [n]
  (let [a (rand-int n)
        b (rand-int n)]
    (if (< a b) (list a b) (list b a))))

(defn split-three-at [i a b]
  (let [p (take a i)
        r (take (- b a) (drop a i))
        q (drop b i)]
    (list p q r)))

(defn split-three [i strlen]
  (let [points (two-points (+ 1 strlen))]
    (println points)
    (split-three-at i (first points) (second points))))

(defn modify-substring [i strlen op]
  (let [substrings (split-three i strlen)]
    (concat (first substrings)
            (op (second substrings))
            (first (rest (rest substrings))))))

(defn sort-ascending [i strlen]
  (modify-substring i strlen
                    (fn [x]
                      (flatten
                        (sort (fn [a b] (< (first a) (first b)))
                              (get-notes x))))))

(defn sort-descending [i strlen]
  (modify-substring i strlen
                    (fn [x]
                      (flatten
                        (sort (fn [a b] (< (first b) (first a)))
                              (get-notes x))))))

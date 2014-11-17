(ns evol.mutation)

(require '[evol.utils :refer :all])

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

(defn inversion [i strlen]
  (flatten 
    (map (fn [x] (cons (- (- MAXNOTE MINNOTE) (first x))
                         (rest x)))
         (get-notes i))))

(defn wiggle-up [i strlen]
  (flatten 
    (map 
      (fn [x] (cons (+ (rand-int 3) (first x)) (rest x)))
      (get-notes i))))

(defn wiggle-down [i strlen]
  (flatten 
    (map 
      (fn [x] (cons (+ (rand-int 3) (first x)) (rest x)))
      (get-notes i))))

(defn random []
  (nth [wiggle-up wiggle-down sort-ascending sort-descending inversion] (rand-int 5)))

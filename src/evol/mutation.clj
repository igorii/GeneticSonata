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

;; TODO (on get-notes)
(defn perfect-cadence [i strlen]
  (concat (take (- strlen 3) i) (list 4 5 0)))

(perfect-cadence (list 1 2 3 4 5 6 7 8) 8)



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
    (map (fn [x] (cons (min MAXNOTE (- MAXNOTE (first x)))
                         (rest x)))
         (get-notes i))))

(defn invert-rhythm [i strlen]
  (map (fn [x] (if (chance 0.8)
                 x
                 (if (hold? x)
                   (from-domain NOTERANGE)
                   HOLD)))
  i))

(defn wiggle-up [i strlen]
  (flatten
    (map
      (fn [x] (cons (min MAXNOTE (+ (rand-int 5) (first x))) (rest x)))
      (get-notes i))))

(defn wiggle-down [i strlen]
  (flatten
    (map
      (fn [x] (cons (max MINNOTE (- (first x) (rand-int 5))) (rest x)))
      (get-notes i))))

(defn random []
  (nth [invert-rhythm wiggle-up wiggle-down perfect-cadence sort-ascending sort-descending inversion]
       (rand-int 7)))

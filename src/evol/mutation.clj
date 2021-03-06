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

(defn perfect-cadence [i strlen]
  (concat (take (- strlen 3) i) (list 4 5 0)))

(defn split-three [i strlen]
  (let [points (two-points (+ 1 strlen))]
    (split-three-at i (first points) (second points))))

(defn modify-substring [i strlen op]
  (let [substrings (split-three i strlen)]
    (concat (first substrings)
            (op (second substrings))
            (first (rest (rest substrings))))))

(defn insert-rests [i strlen]
  (map (fn [x] (if (chance 0.2) HOLD x)) i))

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
  (map (fn [x] (if (chance 0.2)
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
  (nth [insert-rests invert-rhythm wiggle-up wiggle-down sort-ascending sort-descending inversion]
       (rand-int 7)))

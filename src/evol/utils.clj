(ns evol.utils)

(def MAXNOTE 8)
(def MINNOTE -8)
(def NOTERANGE (into [] (range MINNOTE MAXNOTE)))

(def HOLD (- MINNOTE 1))
(defn hold? [x] (= x HOLD))

(defn chance [n] (< (rand) n))
(defn from-domain [d] (d (rand-int (- (.length d) 1))))

(defn get-notes [i]
  (defn inner [i acc]
    (if (empty? i)
      acc
      (if (not (= HOLD (second i)))
        (recur (rest i) (concat acc (list (list (first i)))))
        (let [note (concat (list (first i)) (take-while hold? (rest i)))
              next (drop-while hold? (rest i))]
          (recur next (concat acc (list note)))))))
  (inner i nil))

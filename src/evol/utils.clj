(ns evol.utils)

(def CHORDS (list 0 3 5 4))
(def PHRASELEN (* 8 8))
(def NOTEVAL 1/2)
(def MAXNOTE 8)
(def MINNOTE -8)
(def NOTERANGE (into [] (range MINNOTE MAXNOTE)))

(def HOLD (- MINNOTE 1))
(defn hold? [x] (= x HOLD))
(def REST (+ MAXNOTE 1))
(defn rest? [x] (= x REST))

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

(defn last-bar [melody]
  (reverse (take 8 (reverse melody))))

(defn first-bar [melody]
  (take 8 melody))

(defn max1 [l]
  (reduce (fn [best x] (if (< (first x) (first best)) x best)) (first l) l))
(defn min1 [l]
  (reduce (fn [worst x] (if (> (first x) (first worst)) x worst)) (first l) l))

(defn zip [xs ys] (map list xs ys))

(defn melody->str [melody]
  (apply str (map (fn [x] (if (hold? x) "_" (if (rest? x) "~" (str x)))) melody)))

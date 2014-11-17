(ns evol.utils)

(def MAXNOTE 12)
(def MINNOTE 0)
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

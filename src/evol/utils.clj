(ns evol.utils)

(def CHORDS (list 0 3 5 4))
(def PHRASELEN (* 8 4))
(def NOTEVAL 1/2)
(def MAXNOTE 8)
(def MINNOTE -8)
(def NOTERANGE  (into [] (range MINNOTE MAXNOTE)))
(def CHORDRANGE (into [] (range 0 MAXNOTE)))
(def HOLD (- MINNOTE 1))
(def REST (+ MAXNOTE 1))

(defn hold? [x] (= x HOLD))
(defn rest? [x] (= x REST))

;; Roll a dice
(defn chance [n] (< (rand) n))

;; Retrieve an element from the given domain
(defn from-domain [d] (d (rand-int (- (.length d) 1))))

;; Split a list of notes and holds into a list of lists of the form [(note hold1 hold2...)]
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

;; Get the last measure of a phrase
(defn last-bar [melody]
  (reverse (take 8 (reverse melody))))

;; Get the first measure of a phrase
(defn first-bar [melody]
  (take 8 melody))

;; Get the individuals with the max fitness
(defn max1 [l]
  (reduce (fn [best x] (if (> (first x) (first best)) x best)) (first l) l))

;; Get the individual with the minimum fitness
(defn min1 [l]
  (reduce (fn [worst x] (if (< (first x) (first worst)) x worst)) (first l) l))

;; Zip two lists
(defn zip [xs ys] (map list xs ys))

;; Return a melody in a compact form
(defn melody->str [melody]
  (apply str (map (fn [x] (if (hold? x) "_" (if (rest? x) "~" (str x)))) melody)))

;; Convert a list of notes and holds into a list of note/length pairs
(defn fold-holds [melody lengths]
  (defn f [notes lengths noteacc lengthacc]
    (if (empty? notes)
      (zip noteacc lengthacc)
      (if (hold? (first notes))
        (let [rlengths (reverse lengthacc)]
          (recur (rest notes) (rest lengths) noteacc (reverse (concat (list (+ NOTEVAL (first rlengths))) (rest rlengths)))))
        (recur (rest notes) (rest lengths) (concat noteacc (list (first notes))) (concat lengthacc (list NOTEVAL))))))
  (f melody lengths '() '()))

;; Find the distribution of notes given a list of notes and holds
(defn note-distribution [melody]
  (let [notes (get-notes melody)]
    (sort (reduce (fn [acc x]
                    (assoc acc (count x) (+ 1 (get acc (count x) 0))))
                  {} notes))))

;; Determine the frequency of appearance of notes in a list of notes and holds
(defn note-frequency [melody]
  (let [total (count (get-notes melody))]
    (map (fn [x] [(first x) (/ (second x) total)]) 
         (note-distribution melody))))

;; Instantiate a chord structure on under a theme given a chord rhythm
(defn inst-chords [theme chords]
  (reduce (fn [acc x]
            (if (hold? (second x))
              (concat acc (list (second x)))
              (concat acc (list (first x)))))
          () (zip theme chords)))

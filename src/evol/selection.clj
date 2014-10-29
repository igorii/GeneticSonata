(ns evol.selection)

(defn tournament [fpop size]
  (let* [participants (take size (shuffle fpop))
         winners      (take 2 (sort (fn [fp1 fp2]
                                      (< (first fp1) (first fp2))) participants))]
    winners))

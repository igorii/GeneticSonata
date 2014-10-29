(ns evol.scratch
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord
        overtone.inst.sampled-piano
        [overtone.live :only [at ctl sample freesound-path]]))
(require '[leipzig.melody :refer [bpm is phrase then times where with]])
(require '[overtone.live :as overtone]
         '[leipzig.live :as live]
         '[leipzig.scale :as scale])

(def myscale
  (phrase [1/1 1/2 1/2 1/2 1/2 1/2 1/2 1/2]
          [  0   1   2   3   4   5   6   7]))

myscale
(->> myscale
  (where :time (bpm 90))
  (where :duration (bpm 90))
  (where :pitch (comp scale/C scale/major))
  live/play)
(stop)

(def melody
  (phrase [3/3   3/3   2/3   1/3   3/3]
          [  0     0     0     1     2]))


(def reply "The second bar of the melody."
  (phrase [2/3  1/3  2/3  1/3  6/3]
          [  2    1    2    3    4]))

(def bass "A bass part to accompany the melody."
  (->> (phrase [1  1 2]
               [0 -3 0])
       (where :part (is :bass))))

(overtone/definst beep [freq 440 dur 1.0]
  (-> freq
      overtone/saw
      (* (overtone/env-gen (overtone/perc 0.05 dur) :action overtone/FREE))))

(defmethod live/play-note :default [{midi :pitch seconds :duration}]
  (sampled-piano midi)
  (print midi)
;  (sampled-piano (overtone/midi->hz midi)))
  (-> midi overtone/midi->hz (beep seconds)))

(sampled-piano 70)

(defmethod live/play-note :bass [{midi :pitch}]
  (-> midi overtone/midi->hz (/ 2) (beep 0.5)))

(->>
  bass
  (then (with bass melody))
  (then (with bass melody reply))
  (then (with bass melody reply))
  (where :time (bpm 90))
  (where :duration (bpm 90))
  (where :pitch (comp scale/E scale/minor))
  live/play)


;; Minimal example of how to play a melody using Clojure/Overtone/Leipzig, based on code
;; at https://github.com/ctford/leipzig

;; by Lee Spector, lspector@hampshire.edu, 20140204

;; Add the following to your dependencies in project.cl, and do "lein deps" if your environment requires it:
;; [leipzig "0.7.0"]

(defmethod play-note :default [{midi :pitch}] (sampled-piano midi))

(defn play-melody
  [pitches durations]
  (->> (phrase durations pitches)
    (where :part (is :melody))
    (where :time (bpm 60))
    (where :pitch (comp C major))
    play))

;; C major scale
(play-melody [0 14 2 3 5 5 6 7]  [1 1 1 1 1 1 1 1])
(play-melody [2 3 2 3 4 5 6 7]  [1 1 1 1 1 1 1 1])

;; C chromatic scale
(play-melody [0 0.5 1 1.5 2 3 3.5 4 4.5 5 5.5 6 7]
              [1 1   1 1   1 1 1   1 1   1 1   1 1])

;(stop)

(defn random-beats [n]
  (if (or (<= n 1/8)
          (zero? (rand-int 12)))
    [n]
    (concat (random-beats (/ n 2))
            (random-beats (/ n 2)))))

;(random-beats 4)

;; here's a non-recursive version... uglier but safer:

(defn random-beats [n]
  (loop [result []
         remaining [n]]
    (if (empty? remaining)
      result
      (let [first-thing (first remaining)]
        (if (or (<= first-thing 1/8)
                (zero? (rand-int 12)))
          (recur (conj result first-thing)
                 (rest remaining))
          (recur result
                 (concat [(/ first-thing 2) (/ first-thing 2)]
                         (rest remaining))))))))

;(random-beats 4)

(defn random-pitch []
  (if (zero? (rand-int 8))
    nil
    (- (rand-int 48) 24)))

; (random-pitch)

#_(play-melody (repeatedly random-pitch)
              (random-beats 4))

(let [rhy1 (random-beats 2)
       p1 (repeatedly (count rhy1) random-pitch)
       rhy2 (random-beats 2)
       p2 (repeatedly (count rhy2) random-pitch)
       durations (concat rhy1 rhy1 rhy1 rhy1
                         rhy2 rhy2 rhy1 rhy1)
       pitches (concat p1 p1 p2 p2
                       (reverse p1) (reverse p1) p2 p1)]
   (println "durations:" durations)
   (println "pitches:" pitches)
   (play-melody pitches durations))


;; This will allow you to use a better sounding piano, but the first time you
;; do it it'll take a lot of time to download samples.

;(use 'overtone.inst.sampled-piano)

;(defmethod play-note :default [{midi :pitch}] (sampled-piano midi))

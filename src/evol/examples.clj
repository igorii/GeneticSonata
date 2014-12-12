(ns evol.examples)

(require '[evol.play :refer :all])
(require '[evol.midi :refer :all])

(def song1 [[[0 -9 3 -9 -9 4 5 -9 7 -9 -9 6 4 -9 -9 -9 0 -4 0 1 6 6 -9 7 7 6 6 -9 -9 2 -3 0] [0 -9 -9 -9 3 -9 -9 -9 6 -9 -9 1 3 -9 -9 -9 4 -9 2 -9 1 -9 -9 -9 3 -9 -9 -9 2 3 4 0]] [[0 -9 3 5 4 5 1 0 5 4 8 -9 3 -9 -9 2 6 -9 -9 -9 4 4 0 -1 4 -9 4 -9 0 -9 -2 0] [0 -9 -9 -9 1 -9 -9 -9 2 -9 -9 -9 6 -9 -9 -9 8 -9 3 -9 2 -9 -9 -9 1 -9 -9 -9 3 -9 -9 0]]])
(def song2 [[[-1 -9 4 -9 -9 3 4 -9 8 6 8 -9 -9 -9 5 3 3 4 5 5 -9 2 1 6 7 6 5 3 5 -9 1 0] [1 5 0 -9 1 -9 -9 -9 -3 -9 -9 -9 2 -9 -9 -9 -1 -9 3 -9 -9 -9 2 -9 1 -9 -9 3 -9 -9 -9 1]] [[3 8 -9 -9 7 4 8 7 8 -9 7 -9 -9 -9 8 8 6 -9 7 -9 8 -9 8 -9 6 -9 5 -9 3 5 2 0] [1 -9 -9 -9 2 -9 -9 -9 0 -9 -9 -9 4 -9 -9 2 1 -9 -9 -9 2 -9 0 -9 5 -9 -9 -9 3 -9 -9 0]]])
(def song4 [[[0 -9 3 5 4 5 1 0 5 4 8 -9 3 -9 -9 2 6 -9 -9 -9 4 4 0 -1 4 -9 4 -9 0 -9 -2 0] [0 -9 -9 -9 1 -9 -9 -9 2 -9 -9 -9 6 -9 -9 -9 8 -9 3 -9 2 -9 -9 -9 1 -9 -9 -9 3 -9 -9 0]] [[0 5 4 -9 2 0 2 -9 3 4 3 2 3 -9 4 3 2 3 8 -9 4 2 -9 -9 1 -9 -9 -9 3 -9 0 0] [0 -9 -9 -9 -1 -9 -9 -9 4 -9 -9 -9 8 -9 -9 -9 6 -9 -9 -9 5 -9 3 -9 4 -9 -9 -9 3 -9 -9 0]]])
(def song3 [[[0 -9 5 -9 3 4 -9 -9 3 -9 -9 -9 5 4 5 -9 3 4 4 3 5 -9 4 5 3 2 3 0 4 -9 5 0] [0 -9 -9 -9 1 -9 -9 -9 3 -9 -9 -9 4 -9 -9 -9 3 -9 6 -9 -9 -9 1 -9 3 -9 -9 -9 4 -9 -9 0]] [[0 4 3 -9 2 4 5 -9 3 2 4 -9 5 -9 -9 0 3 -9 -9 -9 2 -9 4 6 6 -9 4 3 2 -9 1 0] [0 -9 1 -9 -2 -9 -9 -9 3 -9 -9 -9 5 -9 -9 -9 4 -9 -9 -9 3 -9 -9 -9 5 -9 -9 -9 4 -9 -9 0]]])
(def song5 [[[0 -9 0 -2 -1 2 0 -9 1 -9 -9 -9 2 -9 1 -9 5 -9 7 8 4 3 5 0 1 2 1 -9 5 -9 -9 0] [0 -9 -1 -9 0 -9 -9 -9 3 -9 -9 -9 -1 -9 -9 -9 0 -9 -9 -9 5 -9 -9 -9 3 -9 -9 -9 1 -9 -9 0]] [[0 -9 -9 -9 5 -9 4 6 7 6 5 -9 4 7 5 6 8 5 4 -9 6 8 8 4 6 8 5 -9 4 -9 -9 0] [0 -9 -9 -9 1 -9 -9 -9 5 -9 -9 -9 7 -9 -9 -9 8 -9 -9 -9 3 -9 5 -9 2 -9 -9 -9 1 -9 -9 0]]])
(def song6 [[[0 -9 0 -9 -9 -9 4 -9 8 -9 -9 6 5 3 8 -9 5 -9 8 -9 8 8 6 8 8 -9 4 -9 0 -5 -3 0] [0 -9 -9 -9 5 -9 -9 -9 1 3 -9 -9 -9 2 -9 -9 1 -9 2 5 1 -9 -9 -9 3 -9 -9 -9 1 -9 -9 0]] [[0 -9 2 0 1 -9 -9 -9 6 -9 4 -9 -9 -9 3 0 1 -9 -9 3 4 -9 0 0 1 5 0 2 0 1 0 -1] [0 -9 -9 -9 4 -9 -9 -9 5 7 8 -9 -9 -9 6 -9 2 5 -9 -9 -9 4 -9 -9 8 -9 -9 -9 3 -9 2 0]]])
(def song7 [[[0 5 6 -9 8 5 3 -9 1 2 -9 3 -9 -9 4 -9 2 1 0 4 5 -9 -9 -9 4 -9 3 4 -9 -9 2 0] [0 4 5 -9 -9 -9 8 -9 3 -9 -9 -9 2 -9 -9 -9 4 -9 -9 -9 2 -9 -9 -9 1 -9 -9 -9 2 -9 -9 0]] nil nil])
(def song8 [[[0 -9 1 -9 6 7 3 2 3 0 2 -9 1 3 -9 1 0 -4 -5 -7 -9 -9 -7 -4 -1 0 0 -9 -9 -9 2 0] [0 -9 5 -9 -9 -9 4 -9 5 8 -9 -9 -9 4 -9 -9 5 -9 -9 -9 7 -9 -9 -9 5 -9 4 -9 -9 -9 0 0]] nil nil])
(def song9 [[[0 -9 -1 -9 4 -9 -9 -9 6 4 5 -9 4 4 5 8 5 7 5 -9 7 6 -9 -9 5 -9 7 -9 8 4 -1 0] [0 -9 3 -9 6 -9 -9 -9 2 -9 -9 -9 5 -9 -9 -9 7 -9 -9 -9 0 -9 -9 -9 3 -9 -9 -9 -3 -9 -9 0]] [[0 -9 4 8 -9 -9 7 -9 8 -9 6 8 7 7 8 7 8 6 8 -9 -9 -9 7 -9 7 8 7 8 6 8 5 0] [0 -9 3 -9 6 -9 -9 -9 2 -9 -9 -9 5 -9 -9 -9 7 -9 -9 -9 0 -9 -9 -9 3 -9 -9 -9 -3 -9 -9 0]] [(list 0 -9 0 -9 1 -2 -5 -2 3 -9 -1 -2 1 -9 0 0 -4 -9 -3 -3 -9 -9 -2 -4 0 -3 -1 -9 -4 -9 0 3 8 5 7 -9 6 -9 8 5 4 6 8 -9 -9 -9 5 7 6 8 5 4 6 5 4 -9 4 5 4 3 5 -9 4 1) (list 0 -9 3 -9 6 -9 -9 -9 2 -9 -9 -9 5 -9 -9 -9 7 -9 -9 -9 0 -9 -9 -9 3 -9 -9 -9 -3 -9 -9 0 0 -9 3 -9 6 -9 -9 -9 2 -9 -9 -9 5 -9 -9 -9 7 -9 -9 -9 0 -9 -9 -9 3 -9 -9 -9 -3 -9 -9 0)]])


(song->midi song9 "song9.mid")

(play-song song2)


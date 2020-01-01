(ns advent2019.day18-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day18 :as t]))

(def d18-s1
  ["#########"
   "#b.A.@.a#"
   "#########"])

(def d18-s2
  ["########################"
   "#f.D.E.e.C.b.A.@.a.B.c.#"
   "######################.#"
   "#d.....................#"
   "########################"])

(def d18-s3
  ["########################"
   "#...............b.C.D.f#"
   "#.######################"
   "#.....@.a.B.c.d.A.e.F.g#"
   "########################"])

(def d18-s4
  ["#################"
   "#i.G..c...e..H.p#"
   "########.########"
   "#j.A..b...f..D.o#"
   "########@########"
   "#k.E..a...g..B.n#"
   "########.########"
   "#l.F..d...h..C.m#"
   "#################"])

(def d18-s5
  ["########################"
   "#@..............ac.GI.b#"
   "###d#e#f################"
   "###A#B#C################"
   "###g#h#i################"
   "########################"])

(deftest shortest-path-test
  (testing "Can find the shortest path to clear the maze"
    (is (= 8   (t/shortest-path (t/load-graph d18-s1))))
    ; (is (= 86  (t/shortest-path d18-s2)))
    ; (is (= 132 (t/shortest-path d18-s3)))
    ; (is (= 136 (t/shortest-path d18-s4)))
    ; (is (= 81  (t/shortest-path d18-s5)))
    ))


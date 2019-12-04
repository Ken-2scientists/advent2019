(ns advent2019.day03-test
  (:require [clojure.test :refer :all]
            [advent2019.day03 :refer :all]))

(def path1-inst '["R8","U5","L5","D3"])
(def path1-trace
  [[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0] [8 0]
   [8 1] [8 2] [8 3] [8 4] [8 5]
   [7 5] [6 5] [5 5] [4 5] [3 5]
   [3 4] [3 3] [3 2]])

(def path2-inst '["U7","R6","D4","L4"])
(def path2-trace
  [[0 0] [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7]
   [1 7] [2 7] [3 7] [4 7] [5 7] [6 7]
   [6 6] [6 5] [6 4] [6 3]
   [5 3] [4 3] [3 3] [3 2]])

(deftest path-trace-test
  (testing "Convert instructions into the wire path"
    (is (= path1-trace (trace-wire-path path1-inst)))))


(def input1 '[["R8","U5","L5","D3"]
              ["U7","R6","D4","L4"]])
(def dist1 6)
(def steps1 30)

(def input2 '[["R75","D30","R83","U83","L12","D49","R71","U7","L72"]
              ["U62","R66","U55","R34","D71","R55","D58","R83"]])
(def dist2 159)
(def steps2 610)

(def input3 '[["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"]
              ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]])
(def dist3 135)
(def steps3 410)

(deftest part1-test
  (testing "Find lower-leftmost intersection of the wire paths"
    (is (= dist1 (closest-intersection-dist input1)))
    (is (= dist2 (closest-intersection-dist input2)))
    (is (= dist3 (closest-intersection-dist input3)))))

(deftest part2-test
  (testing "Find fewest number of steps to first intersection of the wire paths"
    (is (= steps1 (shortest-steps-to-intersection input1)))
    (is (= steps2 (shortest-steps-to-intersection input2)))
    (is (= steps3 (shortest-steps-to-intersection input3)))))
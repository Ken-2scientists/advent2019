(ns advent2019.day17-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day17 :as t]))

(def d17-s1
  (str
   "..#..........\n"
   "..#..........\n"
   "#######...###\n"
   "#.#...#...#.#\n"
   "#############\n"
   "..#...#...#..\n"
   "..#####...^..\n"))

(deftest intersection-test
  (testing "Can find the intersections of the scaffolding"
    (is (= [[2 2] [6 4] [2 4] [10 4]]
           (t/intersections (t/scaffold-map d17-s1))))))

(deftest day17-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 7584 (t/day17-part1-soln)))))

(deftest day17-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 1016738 (t/day17-part2-soln)))))

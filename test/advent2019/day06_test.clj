(ns advent2019.day06-test
  (:require [clojure.test :refer :all]
            [advent2019.day06 :refer :all]))

(deftest orbit-count-test
  (testing "Can correctly count the total number of orbits"
    (is (= 42 (orbit-count {"B" "COM"
                            "C" "B"
                            "D" "C"
                            "E" "D"
                            "F" "E"
                            "G" "B"
                            "H" "G"
                            "I" "D"
                            "J" "E"
                            "K" "J"
                            "L" "K"})))))

(deftest orbit-transfers-test
  (testing "Can correctly determine the number of orbit transfers between YOU and SAN"
    (is (= 4 (orbit-transfers {"B" "COM"
                               "C" "B"
                               "D" "C"
                               "E" "D"
                               "F" "E"
                               "G" "B"
                               "H" "G"
                               "I" "D"
                               "J" "E"
                               "K" "J"
                               "L" "K"
                               "YOU" "K"
                               "SAN" "I"} "YOU" "SAN")))))

(deftest day06-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 261306 (day06-part1-soln)))))

(deftest day06-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 382 (day06-part2-soln)))))

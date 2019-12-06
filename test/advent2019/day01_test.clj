(ns advent2019.day01-test
  (:require [clojure.test :refer :all]
            [advent2019.day01 :refer :all]))

(deftest part1-test
  (testing "Part 1 fuel calculation"
    (is (= 2 (fuel 12)))
    (is (= 2 (fuel 14)))
    (is (= 654 (fuel 1969)))
    (is (= 33583 (fuel 100756)))))

(deftest part2-test
  (testing "Part 2 recursive fuel calculation"
    (is (= 2 (total-fuel 14)))
    (is (= 966 (total-fuel 1969)))
    (is (= 50346 (total-fuel 100756)))))
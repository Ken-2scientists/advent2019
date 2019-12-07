(ns advent2019.day02-test
  (:require [clojure.test :refer :all]
            [advent2019.day02 :refer :all]))

(deftest day02-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 3085697 (day02-part1-soln)))))

(deftest day02-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 9425 (day02-part2-soln 19690720)))))
(ns advent2019.day05-test
  (:require [clojure.test :refer :all]
            [advent2019.day05 :refer :all]))

(deftest day05-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 12234644 (day05-part1-soln)))))

(deftest day05-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 3508186 (day05-part2-soln)))))

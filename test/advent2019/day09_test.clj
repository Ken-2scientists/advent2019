(ns advent2019.day09-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day09 :as t]))

(deftest day09-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 3742852857 (first (t/day09-part1-soln))))))

(deftest day09-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 73439 (first (t/day09-part2-soln))))))

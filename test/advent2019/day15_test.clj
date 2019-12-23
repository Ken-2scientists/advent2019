(ns advent2019.day15-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day15 :as t]))

(deftest day15-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 280 (t/day15-part1-soln)))))


(deftest day15-part2-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 400 (t/day15-part2-soln)))))
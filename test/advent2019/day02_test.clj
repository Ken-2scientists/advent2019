(ns advent2019.day02-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day02 :as t]))

(deftest day02-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 3085697 (t/day02-part1-soln)))))

(deftest day02-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 9425 (t/day02-part2-soln 19690720)))))
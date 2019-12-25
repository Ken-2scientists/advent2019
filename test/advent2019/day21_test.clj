(ns advent2019.day21-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day21 :as t]))

(deftest day21-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 19352864 (t/day21-part1-soln)))))

(deftest day21-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 1142488337 (t/day21-part2-soln)))))

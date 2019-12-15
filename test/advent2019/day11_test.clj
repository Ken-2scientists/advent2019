(ns advent2019.day11-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day11 :as t]))

(deftest day11-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 2539 (t/day11-part1-soln)))))

(ns advent2019.day13-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day13 :as t]))

(deftest day13-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 230 (t/day13-part1-soln)))))
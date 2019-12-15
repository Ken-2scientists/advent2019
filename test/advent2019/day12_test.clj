(ns advent2019.day12-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day12 :as t]))

(def d12-s1
  [[-1 0 2]
   [2 -10 -7]
   [4 -8 8]
   [3 5 -1]])

(def d12-s2
  [[-8 -10 0]
   [5 5 10]
   [2 -7 3]
   [9 -8 -3]])

(deftest total-energy-test
  (testing "Can find the total energy after a number of time steps"
    (is (= 179 (t/total-energy (nth (t/simulate d12-s1) 10))))
    (is (= 1940 (t/total-energy (nth (t/simulate d12-s2) 100))))))
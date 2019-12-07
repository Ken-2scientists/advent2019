(ns advent2019.intcode-test
  (:require [clojure.test :refer :all]
            [advent2019.intcode :refer :all]))

(def input1 [1 9 10 3 2 3 11 0 99 30 40 50])
(def output1 [3500 9 10 70 2 3 11 0 99 30 40 50])

(def input2 [1 0 0 0 99])
(def output2 [2 0 0 0 99])

(def input3 [2 3 0 3 99])
(def output3 [2 3 0 6 99])

(def input4 [2 4 4 5 99 0])
(def output4 [2 4 4 5 99 9801])

(def input5 [1 1 1 4 99 5 6 0 99])
(def output5 [30 1 1 4 2 5 6 0 99])

(deftest simple-op-test
  (testing "Simply add/multiply tests"
    (is (= output1 (first (intcode-ex input1 []))))
    (is (= output2 (first (intcode-ex input2 []))))
    (is (= output3 (first (intcode-ex input3 []))))
    (is (= output4 (first (intcode-ex input4 []))))
    (is (= output5 (first (intcode-ex input5 []))))))
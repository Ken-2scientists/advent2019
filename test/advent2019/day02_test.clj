(ns advent2019.day02-test
  (:require [clojure.test :refer :all]
            [advent2019.day02 :refer :all]))


(def input1 '[1 9 10 3 2 3 11 0 99 30 40 50])
(def output1 '[3500 9 10 70 2 3 11 0 99 30 40 50])

(def input2 '[1 0 0 0 99])
(def output2 '[2 0 0 0 99])

(def input3 '[2 3 0 3 99])
(def output3 '[2 3 0 6 99])

(def input4 '[2 4 4 5 99 0])
(def output4 '[2 4 4 5 99 9801])

(def input5 '[1 1 1 4 99 5 6 0 99])
(def output5 '[30 1 1 4 2 5 6 0 99])


(deftest part1-test
  (testing "Apply opcode execution"
    (is (= output1 (opcode-ex input1)))
    (is (= output2 (opcode-ex input2)))
    (is (= output3 (opcode-ex input3)))
    (is (= output4 (opcode-ex input4)))
    (is (= output5 (opcode-ex input5)))))

(deftest part2-test
  (testing "Can find the noun and verb from part 1"
    (is (= 1202 (day02-part2-soln 3085697)))))
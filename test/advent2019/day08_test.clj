(ns advent2019.day08-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day08 :as t]))

(deftest day08-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 2016 (t/day08-part1-soln)))))

(def decoded-output
  '(1 0 0 1 0 1 1 1 1 0 0 1 1 0 0 1 1 1 1 0 1 0 0 1 0
      1 0 0 1 0 0 0 0 1 0 1 0 0 1 0 0 0 0 1 0 1 0 0 1 0
      1 1 1 1 0 0 0 1 0 0 1 0 0 0 0 0 0 1 0 0 1 0 0 1 0
      1 0 0 1 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 1 0
      1 0 0 1 0 1 0 0 0 0 1 0 0 1 0 1 0 0 0 0 1 0 0 1 0
      1 0 0 1 0 1 1 1 1 0 0 1 1 0 0 1 1 1 1 0 0 1 1 0 0))

(deftest space-image-decoder-test
  (testing "Can decode an image in space image format"
    (is (= '(0 1 1 0) (t/decode-image [0 2 2 2 1 1 2 2 2 2 1 2 0 0 0 0] 2 2)))
    (is (= decoded-output (t/decode-image t/day08-input 25 6)))))
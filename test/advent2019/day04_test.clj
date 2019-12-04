(ns advent2019.day04-test
  (:require [clojure.test :refer :all]
            [advent2019.day04 :refer :all]))

(deftest not-decreasing-condition
  (testing "Can assert a number has increasing digits"
    (is (not-decreasing-digits? (digits 122345)))
    (is (not-decreasing-digits? (digits 111123)))
    (is (not-decreasing-digits? (digits 135679)))
    (is (not (not-decreasing-digits? (digits 223450))))))

(deftest has-one-pair-condition
  (testing "Can assert a number has at least one set of consecutive matching digits"
    (is (one-matching-pair? (digits 122345)))
    (is (one-matching-pair? (digits 111111)))
    (is (one-matching-pair? (digits 223450)))
    (is (not (one-matching-pair? (digits 123789))))))
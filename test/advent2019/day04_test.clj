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

(deftest pairs-not-in-larger-group-condition
  (testing "Consecutive same values can only come in even-sized runs"
    (is (no-larger-groups? (digits 112233)))
    (is (not (no-larger-groups? (digits 123444))))
    (is (no-larger-groups? (digits 111122)))))

(deftest day04-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 1330 (day04-part1-soln)))))

(deftest day01-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 876 (day04-part2-soln)))))
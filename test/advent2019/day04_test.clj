(ns advent2019.day04-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day04 :as t]))

(deftest not-decreasing-condition
  (testing "Can assert a number has increasing digits"
    (is (t/not-decreasing-digits? (t/digits 122345)))
    (is (t/not-decreasing-digits? (t/digits 111123)))
    (is (t/not-decreasing-digits? (t/digits 135679)))
    (is (not (t/not-decreasing-digits? (t/digits 223450))))))

(deftest has-one-pair-condition
  (testing "Can assert a number has at least one set of consecutive matching digits"
    (is (t/one-matching-pair? (t/digits 122345)))
    (is (t/one-matching-pair? (t/digits 111111)))
    (is (t/one-matching-pair? (t/digits 223450)))
    (is (not (t/one-matching-pair? (t/digits 123789))))))

(deftest pairs-not-in-larger-group-condition
  (testing "Consecutive same values can only come in even-sized runs"
    (is (t/pair-not-in-larger-group? (t/digits 112233)))
    (is (not (t/pair-not-in-larger-group? (t/digits 123444))))
    (is (t/pair-not-in-larger-group? (t/digits 111122)))))

(deftest day04-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 1330 (t/day04-part1-soln)))))

(deftest day01-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 876 (t/day04-part2-soln)))))
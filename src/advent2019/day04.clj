(ns advent2019.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set]))

(def any? (complement not-any?))

(defn digits
  "Converts an integer into a seq of its digits in order"
  [num]
  (map (comp read-string str) (str num)))

(defn six-digit?
  [digits]
  (= 6 (count digits)))

(defn not-decreasing-digits?
  [digits]
  (every? (complement neg?) (map - (rest digits) digits)))

(defn matching-pair?
  [[a b]]
  (= a b))

(defn one-matching-pair?
  [digits]
  (any? matching-pair? (partition 2 1 digits)))

(def all-conds?
  (every-pred six-digit? not-decreasing-digits? one-matching-pair?))

(defn satisfactory-numbers
  [numbers]
  (->> numbers
       (map digits)
       (filter all-conds?)))

(defn day04-part1-soln
  []
  (count (satisfactory-numbers (range 231832 767346))))

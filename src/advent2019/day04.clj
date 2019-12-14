(ns advent2019.day04)

(def my-any? (complement not-any?))

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

(defn one-matching-pair?
  [digits]
  (my-any? #(>= % 2) (vals (frequencies digits))))

(def all-conds-part1?
  (every-pred six-digit? not-decreasing-digits? one-matching-pair?))

(defn satisfactory-numbers
  [numbers condition]
  (->> numbers
       (map digits)
       (filter condition)))

(defn day04-part1-soln
  []
  (count (satisfactory-numbers (range 231832 767346) all-conds-part1?)))

(defn pair-not-in-larger-group?
  [digits]
  (my-any? #(= % 2) (vals (frequencies digits))))

(def all-conds-part2?
  (every-pred six-digit? not-decreasing-digits? pair-not-in-larger-group?))

(defn day04-part2-soln
  []
  (count (satisfactory-numbers (range 231832 767346) all-conds-part2?)))
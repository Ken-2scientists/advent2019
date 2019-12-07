(ns advent2019.day05
  (:require [advent2019.intcode :as intcode]
            [advent2019.utils :as u]))

(def day05-input (-> "day05-input.txt" u/load-file first u/list-line))

(defn day05-part1-soln
  []
  (last (intcode/intcode-ex day05-input [1])))

(defn day05-part2-soln
  []
  (first (intcode/intcode-ex day05-input [5])))
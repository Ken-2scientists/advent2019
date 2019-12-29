(ns advent2019.day09
  (:require [advent2019.lib.intcode :as intcode]
            [advent2019.lib.utils :as u]))

(def day09-input (u/puzzle-input-vec "day09-input.txt"))

(defn day09-part1-soln
  []
  (intcode/read-output (intcode/intcode-ex day09-input [1])))

(defn day09-part2-soln
  []
  (intcode/read-output (intcode/intcode-ex day09-input [2])))

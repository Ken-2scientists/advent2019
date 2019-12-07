(ns advent2019.day05
  (:require [clojure.java.io :as io]
            [advent2019.intcode :as intcode]))

(def day05-input
  (-> "day05-input.txt"
      io/resource
      slurp
      intcode/parse-intcode))

(defn day05-part1-soln
  []
  (last (intcode/intcode-ex day05-input [1])))

(defn day05-part2-soln
  []
  (first (intcode/intcode-ex day05-input [5])))
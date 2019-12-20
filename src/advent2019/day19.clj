(ns advent2019.day19
  (:require   [advent2019.intcode :as intcode]
              [advent2019.utils :as u]))

(def day19-input (u/puzzle-input-vec "day19-input.txt"))

(defn tractor-beam
  [intcode size]
  (for [y (range size)
        x (range size)]
    (intcode/read-output (intcode/intcode-ex intcode [x y]))))

(defn day19-part1-soln
  []
  (count (filter pos? (flatten (tractor-beam day19-input 50)))))
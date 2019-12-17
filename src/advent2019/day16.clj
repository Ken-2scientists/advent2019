(ns advent2019.day16
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

(def day16-input
  (map (comp read-string str) (first (u/puzzle-input "day16-input.txt"))))

(def pattern [0 1 0 -1])

(defn digit-calc
  [num pattern]
  (mod (Math/abs (reduce + (map * num pattern))) 10))

(defn phase
  [input]
  (let [nums (map (comp read-string str) input)
        size (count nums)
        patterns (for [x (range size)]
                   (rest (cycle (mapcat (partial repeat (inc x)) pattern))))]
    (str/join (map (partial digit-calc nums) patterns))))

(defn day16-part1-soln
  []
  (subs (nth (iterate phase day16-input) 100) 0 8))


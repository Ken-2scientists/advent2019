(ns advent2019.day01
  (:require [advent2019.lib.utils :as u]))

(def day01-input
  (map read-string (u/puzzle-input "day01-input.txt")))

(defn fuel
  [m]
  (let [f (-> m
              (/ 3)
              Math/floor
              (- 2)
              int)]
    (if (neg? f) 0 f)))

(defn total-fuel
  [m]
  (loop [nxt m total 0]
    (if (zero? nxt)
      total
      (let [fuel-mass (fuel nxt)]
        (recur fuel-mass (+ total fuel-mass))))))

(defn compute-total
  [fuel-calc]
  (->> day01-input
       (map fuel-calc)
       (reduce +)))

(defn day01-part1-soln
  []
  (compute-total fuel))

(defn day01-part2-soln
  []
  (compute-total total-fuel))
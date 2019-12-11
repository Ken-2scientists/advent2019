(ns advent2019.day10
  (:require [advent2019.utils :as u]))

(defn parse-map
  [ascii-asteroids]
  (for [y (range (count ascii-asteroids))
        x (range (count (first ascii-asteroids)))
        :when (= \# (nth (nth ascii-asteroids y) x))]
    [x y]))

(def day10-input (parse-map (u/puzzle-input "day10-input.txt")))

(defn quadrant
  [[x1 y1] [x2 y2]]
  (let [deltay (- y1 y2)
        deltax (- x2 x1)]
    (if (pos? deltay)
      (if (pos? deltax)
        :q1
        :q2)
      (if (pos? deltax)
        :q4
        :q3))))

(defn slope
  "Slope between two points, using coordinates where y-values increase downward"
  [[x1 y1] [x2 y2]]
  (cond
    (= x1 x2) (if (> y1 y2)
                :up
                :down)
    (= y1 y2) (if (> x2 x1)
                :right
                :left)
    :else [(/ (- y1 y2) (- x2 x1)) (quadrant [x1 y1] [x2 y2])]))

(defn visible-asteroid-count
  "Find all asteroids in others that are visible to x"
  [x others]
  (let [others-not-x (filter #(not= x %) others)
        slopes (map (partial slope x) others-not-x)]
    (count (set slopes))))

(defn best-location
  [asteroids]
  (let [counts (map #(visible-asteroid-count % asteroids) asteroids)
        visibles (zipmap asteroids counts)]
    (apply max-key second visibles)))

(defn day10-part1-soln
  []
  (best-location day10-input))

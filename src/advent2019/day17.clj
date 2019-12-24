(ns advent2019.day17
  (:require [clojure.string :as str]
            [advent2019.intcode :as intcode]
            [advent2019.maze :as maze]
            [advent2019.utils :as u]))

(def day17-input (u/puzzle-input-vec "day17-input.txt"))

(defn char->obj
  [char]
  (case char
    \. :space
    \# :scaffold
    \^ :robot
    :unknown))

(defn scaffold-map
  [ascii]
  (let [lines (str/split (str/join (map char ascii)) #"\n")
        row-count (count lines)
        symbols (map #(map char->obj %) lines)
        col-count (count (first symbols))]
    (zipmap (for [y (range row-count)
                  x (range col-count)]
              [x y])
            (flatten symbols))))

(defn intersection?
  [space pos]
  (if (= :scaffold (space pos))
    (every? #(= :scaffold %) (vals (maze/better-neighbors space pos)))
    false))

(defn intersections
  [space]
  (let [candidates (filter (partial intersection? space) (keys space))]
    candidates))

(defn alignment-sum
  [intersections]
  (reduce + (map #(apply * %) intersections)))

(defn day17-part1-soln
  []
  (->> (intcode/intcode-ex day17-input [])
       intcode/read-output
       scaffold-map
       intersections
       alignment-sum))
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
  [intcode]
  (let [output (intcode/read-output (intcode/intcode-ex intcode []))
        lines (str/split (str/join (map char output)) #"\n")
        row-count (count lines)
        symbols (map #(map char->obj %) lines)
        col-count (count (first symbols))]
    (println row-count col-count)
    (zipmap (for [y (range col-count)
                  x (range row-count)]
              [x y])
            (flatten symbols))))


(defn intersection?
  [space pos]
  (if (= :scaffold (space pos))
    (every? #(= :scaffold) (vals (maze/better-neighbors space pos)))
    false))

(defn intersections
  [space]
  (let [candidates (filter #(intersection? space %) (keys space))]
    candidates))
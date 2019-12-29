(ns advent2019.day03
  (:require [clojure.string :as str]
            [clojure.set]
            [advent2019.lib.utils :as u]))

(def day03-input
  (->> (u/puzzle-input "day03-input.txt")
       (map #(str/split % #","))))

(defn expand-wire-segment
  [pos instruction]
  (let [[x0 y0] pos
        dir (str (first instruction))
        length (read-string (str/join (drop 1 instruction)))]
    [dir length]
    (->
     (case dir
       "U" (for [y (range (+ y0 1) (+ (+ y0 length) 1))] [x0 y])
       "D" (for [y (range (- y0 1) (- (- y0 length) 1) -1)] [x0 y])
       "R" (for [x (range (+ x0 1) (+ (+ x0 length) 1))] [x y0])
       "L" (for [x (range (- x0 1) (- (- x0 length) 1) -1)] [x y0]))
     vec)))

(defn trace-wire-path
  [instructions]
  (loop [insts instructions path [[0 0]]]
    (let [last-pos (last path)
          next-inst (first insts)]
      (if (not insts)
        path
        (recur (next insts) (into path (expand-wire-segment last-pos next-inst)))))))

(defn intersections
  [[wire1 wire2]]
  (let [path1 (set (trace-wire-path wire1))
        path2 (set (trace-wire-path wire2))]
    (clojure.set/intersection path1 path2)))

(defn metropolitan-dist
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn closest-intersection-dist
  [instructions]
  (->> (intersections instructions)
       (map metropolitan-dist)
       sort
       ; we take second because [0 0] is in the set
       second))

(defn day03-part1-soln
  []
  (closest-intersection-dist day03-input))

; Slightly convoluted. I want a mapping from a point
; to the shortest distance to that point. Because 
; later values clobber earlier ones, I traverse the
; path backwards so the shortest distance to a single
; point remains in the map at the end
(defn shortest-distance-to-point
  [path]
  (let [len (count path)]
    (zipmap (reverse path) (reverse (range len)))))

(defn distances-to-intersections
  [[wire1 wire2]]
  (let [path1 (trace-wire-path wire1)
        path2 (trace-wire-path wire2)
        path1-dists (shortest-distance-to-point path1)
        path2-dists (shortest-distance-to-point path2)
        ints (clojure.set/intersection (set path1) (set path2))]
    (map (fn [pos] [pos (+ (get path1-dists pos) (get path2-dists pos))]) ints)))

(defn shortest-steps-to-intersection
  [[wire1 wire2]]
  (->> (distances-to-intersections [wire1 wire2])
       (sort-by second)
       second
       second))

(defn day03-part2-soln
  []
  (shortest-steps-to-intersection day03-input))
(ns advent2019.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set]))

(def day03-input
  (->> "day03-input.txt"
       io/resource
       io/reader
       line-seq
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

(defn closest-intersection-dist
  [instructions]
  (->> (intersections instructions)
       sort
       second ; we take second because [0 0] is in the set
       (reduce +)))

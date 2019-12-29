(ns advent2019.day02
  (:require [advent2019.lib.intcode :as intcode]
            [advent2019.lib.utils :as u]))

(def day02-input (u/puzzle-input-vec "day02-input.txt"))

(defn override-intcode
  [intcode noun verb]
  (-> intcode
      (assoc 1 noun)
      (assoc 2 verb)))

(defn day02-part1-soln
  []
  (let [intcode (override-intcode day02-input 12 2)]
    (first (:intcode (intcode/intcode-ex intcode [])))))

(defn day02-part2-soln
  [value]
  (let [search-space (for [noun (range 100)
                           verb (range 100)]
                       (let [intcode (override-intcode day02-input noun verb)]
                         [noun verb (first (:intcode (intcode/intcode-ex intcode [])))]))
        match (first (filter #(= value (nth % 2)) search-space))
        [noun verb] match]
    (+ (* 100 noun) verb)))



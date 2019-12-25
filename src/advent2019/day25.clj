(ns advent2019.day25
  (:require [clojure.math.combinatorics :as combo]
            [advent2019.intcode :as intcode]
            [advent2019.utils :as u]))

(def day25-input (u/puzzle-input-vec "day25-input.txt"))
(def day25-cmds (u/puzzle-input "day25-cmds.txt"))

(def checkpoint1-items
  ["astronaut ice cream"
   "coin"
   "dark matter"
   "festive hat"
   "klein bottle"
   "mutex"
   "pointer"
   "whirled peas"])

(defn test-combo
  [combo]
  (concat (map #(str "take " %) combo)
          ["east"]
          (map #(str "drop " %) combo)))

(defn test-checkpoint-cmds
  [inventory]
  (let [drop-all (map #(str "drop " %) inventory)
        combo-attempts (mapcat test-combo (mapcat #(combo/combinations inventory %) (range 1 7)))]
    (concat drop-all combo-attempts)))

(defn test-combinations
  []
  (intcode/interactive-asciicode
   day25-input
   (concat day25-cmds (test-checkpoint-cmds checkpoint1-items))))

(defn day25-part1-soln
  []
  (intcode/interactive-asciicode
   day25-input
   (concat day25-cmds
           ["drop dark matter"]
           ["drop astronaut ice cream"]
           ["drop klein bottle"]
           ["drop pointer"]
           ["east"])))
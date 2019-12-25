(ns advent2019.day21
  (:require [clojure.string :as str]
            [advent2019.ascii :as ascii]
            [advent2019.intcode :as intcode]
            [advent2019.maze :as maze]
            [advent2019.utils :as u]))

(def day21-input (u/puzzle-input-vec "day21-input.txt"))

(def spring-codes-part1
  ["OR A T"
   "AND B T"
   "AND C T"
   "NOT T J"
   "AND D J"
   "WALK"])

(def spring-codes-part2
  ["OR A T"
   "AND B T"
   "AND C T"
   "NOT T J"
   "AND D J"
   "RUN"])

(defn interactive
  []
  (intcode/interactive-asciicode day21-input []))

(defn day21-part1-soln
  []
  (last (intcode/read-output (intcode/intcode-ex day21-input (intcode/cmds->ascii spring-codes-part1)))))

(defn day21-part2-soln
  []
  (intcode/read-output (intcode/intcode-ex day21-input (intcode/cmds->ascii spring-codes-part2))))
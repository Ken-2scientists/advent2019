(ns advent2019.day21
  (:require [advent2019.lib.intcode :as intcode]
            [advent2019.lib.utils :as u]))

(def day21-input (u/puzzle-input-vec "day21-input.txt"))

(def spring-codes-part1
  ["OR A T"
   "AND B T"
   "AND C T"
   "NOT T J"
   "AND D J"
   "WALK"])

(def spring-codes-part2
  ["OR A J"
   "AND B J"
   "AND C J"
   "NOT J J"
   "AND D J"
   "OR E T"
   "OR H T"
   "AND T J"
   "RUN"])

(defn interactive
  []
  (intcode/interactive-asciicode day21-input []))

(defn day21-part1-soln
  []
  (last (intcode/read-output (intcode/intcode-ex day21-input (intcode/cmds->ascii spring-codes-part1)))))

(defn day21-part2-soln
  []
  (last (intcode/read-output (intcode/intcode-ex day21-input (intcode/cmds->ascii spring-codes-part2)))))
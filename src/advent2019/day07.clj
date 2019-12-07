(ns advent2019.day07
  (:require [clojure.java.io :as io]
            [advent2019.intcode :as intcode]
            [clojure.math.combinatorics :as combo]))

(def day07-input
  (-> "day07-input.txt"
      io/resource
      slurp
      intcode/parse-intcode))

(def sample1 [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
(def sample2 [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0])
(def sample3 [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])

(defn amplifier-chain
  [intcode phases]
  (loop [ph phases cnt 0 out 0]
    (if (= cnt 5)
      out
      (let [nextout (first (intcode/intcode-ex intcode [(first ph) out]))]
        (recur (rest ph) (inc cnt) nextout)))))

(defn find-max-phases
  [intcode]
  (let [search-space (combo/permutations [0 1 2 3 4])
        outputs (map (fn [p] [p (amplifier-chain intcode p)]) search-space)]
    (apply max-key second outputs)))

(defn day07-part1-soln
  []
  (second (find-max-phases day07-input)))
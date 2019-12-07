(ns advent2019.day07
  (:require [clojure.math.combinatorics :as combo]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [advent2019.intcode :as intcode]
            [advent2019.utils :as u]))

(def day07-input (u/puzzle-input-vec "day07-input.txt"))

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

(def sample4 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
(def sample5 [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])

(defn amplifier-loop
  [intcode phases]
  (let [a-in (s/stream)
        b-in (s/stream)
        c-in (s/stream)
        d-in (s/stream)
        e-in (s/stream)]
    (do (s/put! a-in (nth phases 0))
        (s/put! b-in (nth phases 1))
        (s/put! c-in (nth phases 2))
        (s/put! d-in (nth phases 3))
        (s/put! e-in (nth phases 4))
        (s/put! a-in 0)
        (d/future (intcode/intcode-ex-async intcode a-in b-in))
        (d/future (intcode/intcode-ex-async intcode b-in c-in))
        (d/future (intcode/intcode-ex-async intcode c-in d-in))
        (d/future (intcode/intcode-ex-async intcode d-in e-in))
        @(d/future (intcode/intcode-ex-async intcode e-in a-in))
        @(s/take! a-in))))

(defn find-max-phases-part2
  [intcode]
  (let [search-space (combo/permutations [5 6 7 8 9])
        outputs (map (fn [p] [p (amplifier-loop intcode p)]) search-space)]
    (apply max-key second outputs)))

(defn day07-part2-soln
  []
  (second (find-max-phases-part2 day07-input)))
(ns advent2019.day07
  (:require [clojure.math.combinatorics :as combo]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [advent2019.intcode :as intcode]
            [advent2019.utils :as u]))

(def day07-input (u/puzzle-input-vec "day07-input.txt"))

(defn run-intcode
  [intcode [in out]]
  (d/future (intcode/intcode-ex-async intcode in out)))

(defn amplifier-chain
  [intcode phases]
  (let [streams (for [_ (range (inc (count phases)))] (s/stream))]
    (doall (map s/put! streams phases))
    (s/put! (first streams) 0)
    (let [futures (doall (map (partial run-intcode intcode) (partition 2 1 streams)))]
      @(last futures))
    @(s/take! (last streams))))

(defn amplifier-loop
  [intcode phases]
  (let [streams (for [_ (range (count phases))] (s/stream))]
    (doall (map s/put! streams phases))
    (s/put! (first streams) 0)
    (let [futures (doall (map (partial run-intcode intcode) (partition 2 1 streams streams)))]
      @(last futures))
    @(s/take! (first streams))))

(defn max-phases
  [intcode amplifier phase-options]
  (let [search-space (combo/permutations phase-options)
        outputs (map (fn [p] [p (amplifier intcode p)]) search-space)]
    (apply max-key second outputs)))

(defn amplifier-chain-max-phases
  [intcode]
  (max-phases intcode amplifier-chain [0 1 2 3 4]))

(defn amplifier-loop-max-phases
  [intcode]
  (max-phases intcode amplifier-loop [5 6 7 8 9]))

(defn day07-part1-soln
  []
  (second (amplifier-chain-max-phases day07-input)))

(defn day07-part2-soln
  []
  (second (amplifier-loop-max-phases day07-input)))
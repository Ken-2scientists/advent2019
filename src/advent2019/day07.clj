(ns advent2019.day07
  (:require [clojure.math.combinatorics :as combo]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [advent2019.intcode :as intcode]
            [advent2019.utils :as u]))

(def day07-input (u/puzzle-input-vec "day07-input.txt"))

; (defn amplifier-chain
;   [intcode phases]
;   (loop [ph phases cnt 0 out 0]
;     (if (= cnt 5)
;       out
;       (let [nextout (first (intcode/intcode-ex intcode [(first ph) out]))]
;         (recur (rest ph) (inc cnt) nextout)))))

(defn amplifier-chain
  [intcode phases]
  (let [a-in (s/stream)
        b-in (s/stream)
        c-in (s/stream)
        d-in (s/stream)
        e-in (s/stream)
        out  (s/stream)]
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
        @(d/future (intcode/intcode-ex-async intcode e-in out))
        @(s/take! out))))

(defn amplifier-chain-max-phases
  [intcode]
  (let [search-space (combo/permutations [0 1 2 3 4])
        outputs (map (fn [p] [p (amplifier-chain intcode p)]) search-space)]
    (apply max-key second outputs)))

(defn day07-part1-soln
  []
  (second (amplifier-chain-max-phases day07-input)))

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

(defn amplifier-loop-max-phases
  [intcode]
  (let [search-space (combo/permutations [5 6 7 8 9])
        outputs (map (fn [p] [p (amplifier-loop intcode p)]) search-space)]
    (apply max-key second outputs)))

(defn day07-part2-soln
  []
  (second (amplifier-loop-max-phases day07-input)))
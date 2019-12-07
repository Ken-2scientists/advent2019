(ns advent2019.day02
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

(def day02-input (-> "day02-input.txt" u/load-file first u/list-line))

(defn apply-op
  [opcode pos]
  (let [instruction (subvec opcode pos (+ pos 4))
        [op in-pos1 in-pos2 out-pos] instruction
        in-val1 (nth opcode in-pos1)
        in-val2 (nth opcode in-pos2)
        op (nth opcode (+ pos 0))
        new-val (case op
                  1 (+ in-val1 in-val2)
                  2 (* in-val1 in-val2))]
    (assoc opcode out-pos new-val)))

(defn opcode-ex
  [opcode]
  (loop [ops opcode pos 0]
    (if (= 99 (nth ops pos))
      ops
      (recur (apply-op ops pos) (+ 4 pos)))))

(defn set-opcode-inputs
  [opcode noun verb]
  (-> opcode
      (assoc 1 noun)
      (assoc 2 verb)))

(defn day02-part1-soln
  []
  (let [opcode (set-opcode-inputs day02-input 12 2)]
    (first (opcode-ex opcode))))

(defn day02-part2-soln
  [value]
  (let [search-space (for [noun (range 100)
                           verb (range 100)]
                       (let [opcode (set-opcode-inputs day02-input noun verb)]
                         [noun verb (first (opcode-ex opcode))]))
        match (first (filter #(= value (nth % 2)) search-space))
        [noun verb] match]
    (+ (* 100 noun) verb)))



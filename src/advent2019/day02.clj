(ns advent2019.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day02-input
  (let [str-vals (-> "day02-input.txt"
                     io/resource
                     slurp
                     (str/split #","))]
    (map read-string str-vals)))

(defn apply-op
  [opcode pos]
  (let [op (nth opcode (+ pos 0))
        in-val1 (nth opcode (nth opcode (+ pos 1)))
        in-val2 (nth opcode (nth opcode (+ pos 2)))
        out-pos (nth opcode (+ pos 3))
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



(ns advent2019.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day05-input
  (let [str-vals (-> "day05-input.txt"
                     io/resource
                     slurp
                     (str/split #","))]
    (vec (map read-string str-vals))))

(def ops {1 {:name :add :params 3}
          2 {:name :multiply :params 3}
          3 {:name :read :params 1}
          4 {:name :write :params 1}
          99 {:name :stop :params 0}})

(defn param-type
  [param-str]
  (case param-str
    \1 :immediate
    :position))

(defn parse-instruction
  [instruction]
  (let [opcode (mod instruction 100)
        operation (get-in ops [opcode :name])
        params (get-in ops [opcode :params])
        str-size   (+ params 2)
        format-str (str "%0" str-size "d")
        opstr  (format format-str instruction)
        param-codes (subs (str/reverse opstr) 2 str-size)]
    {:operation operation
     :params params
     :size  (+ params 1)
     :param-types (map param-type param-codes)}))

; (defn apply-op
;   [opcode pos]
;   (let [instruction (subvec opcode pos (+ pos 4))
;         [op in-pos1 in-pos2 out-pos] instruction
;         in-val1 (nth opcode in-pos1)
;         in-val2 (nth opcode in-pos2)
;         op (nth opcode (+ pos 0))
;         new-val (case op
;                   1 (+ in-val1 in-val2)
;                   2 (* in-val1 in-val2))]
;     (assoc opcode out-pos new-val)))

; (defn opcode-ex
;   [opcode]
;   (loop [ops opcode pos 0]
;     (if (= 99 (nth ops pos))
;       ops
;       (recur (apply-op ops pos) (+ 4 pos)))))





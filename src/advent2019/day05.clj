(ns advent2019.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day05-input
  (let [str-vals (-> "day05-input.txt"
                     io/resource
                     slurp
                     (str/split #","))]
    (vec (map read-string str-vals))))

(defn read-param
  [intcode type value]
  (case type
    :immediate value
    :position (nth intcode value)))

(defn write-param
  [intcode pos value]
  (assoc intcode pos value))

(defn add
  [intcode [t1 t2 _] [v1 v2 v3]]
  (write-param intcode v3 (+ (read-param intcode t1 v1)
                             (read-param intcode t2 v2))))

(defn multiply
  [intcode [t1 t2 _] [v1 v2 v3]]
  (write-param intcode v3 (* (read-param intcode t1 v1)
                             (read-param intcode t2 v2))))

(defn input
  [intcode [t1] [v1]]
  (write-param intcode v1 (read-string (read-line))))

(defn output
  [intcode [t1] [v1]]
  (do
    (println (read-param intcode t1 v1))
    intcode))

(defn jump-if-true
  [intcode [t1 t2] [v1 v2]]
  ; TODO
  intcode)

(defn jump-if-false
  [intcode [t1 t2] [v1 v2]]
  ; TODO
  intcode)

(defn less-than
  [intcode [t1 t2 _] [v1 v2 v3]]
  (write-param intcode v3 (if (< (read-param intcode t1 v1)
                                 (read-param intcode t2 v2))
                            1
                            0)))

(defn equals
  [intcode [t1 t2 _] [v1 v2 v3]]
  (write-param intcode v3 (if (= (read-param intcode t1 v1)
                                 (read-param intcode t2 v2))
                            1
                            0)))

(def ops {1 {:name :add :op add :param-count 3 :size 4}
          2 {:name :multiply :op multiply :param-count 3 :size 4}
          3 {:name :input :op input :param-count 1 :size 2}
          4 {:name :output :op output :param-count 1 :size 2}
          5 {:name :jump-if-true :op jump-if-true :param-count 2 :size 3}
          6 {:name :jump-if-false :op jump-if-false :param-count 2 :size 3}
          7 {:name :less-than :op less-than :param-count 3 :size 4}
          8 {:name :equals :op equals :param-count 3 :size 4}
          99 {:name :stop :param-count 0 :size 1}})

(defn param-type
  [param-str]
  (case param-str
    \1 :immediate
    :position))

(defn parse-instruction
  [instruction]
  (let [operation (get ops (mod instruction 100))
        {:keys [param-count]} operation
        str-size   (+ param-count 2)
        format-str (str "%0" str-size "d")
        opstr  (format format-str instruction)
        param-codes (subs (str/reverse opstr) 2 str-size)]
    (assoc operation :param-types (map param-type param-codes))))

(defn apply-op
  [intcode pos]
  (let [instruction (parse-instruction (nth intcode pos))
        {:keys [op name size param-types]} instruction
        args (subvec intcode (inc pos) (+ pos size))]
    [(op intcode param-types args) (+ pos size)]))

(defn intcode-ex
  [intcode]
  (loop [newintcode intcode pos 0]
    (if (= 99 (nth newintcode pos))
      newintcode
      (let [[nextcode nextpos] (apply-op newintcode pos)]
        (recur nextcode nextpos)))))





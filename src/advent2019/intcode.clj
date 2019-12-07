(ns advent2019.intcode
  (:require [clojure.string :as str]))

(defn parse-intcode
  [intcode-str]
  (->> (str/split intcode-str #",")
       (map read-string)
       vec))

(defn read-param
  [intcode type value]
  (case type
    :immediate value
    :position (nth intcode value)))

(defn write-param
  [intcode pos value]
  (assoc intcode pos value))

(defn add
  [intcode pos [t1 t2 _] [v1 v2 v3] in out]
  [(write-param intcode v3 (+ (read-param intcode t1 v1)
                              (read-param intcode t2 v2)))
   (+ 4 pos)
   in
   out])

(defn multiply
  [intcode pos [t1 t2 _] [v1 v2 v3] in out]
  [(write-param intcode v3 (* (read-param intcode t1 v1)
                              (read-param intcode t2 v2)))
   (+ 4 pos)
   in
   out])

(defn input
  [intcode pos [t1] [v1] in out]
  [(write-param intcode v1 (first in))
   (+ 2 pos)
   (next in)
   out])

(defn output
  [intcode pos [t1] [v1] in out]
  [intcode
   (+ 2 pos)
   in
   (conj out (read-param intcode t1 v1))])

(defn jump-if-true
  [intcode pos [t1 t2] [v1 v2] in out]
  (let [nextpos (if (zero? (read-param intcode t1 v1))
                  (+ 3 pos)
                  (read-param intcode t2 v2))]
    [intcode nextpos in out]))

(defn jump-if-false
  [intcode pos [t1 t2] [v1 v2] in out]
  (let [nextpos (if (zero? (read-param intcode t1 v1))
                  (read-param intcode t2 v2)
                  (+ 3 pos))]
    [intcode nextpos in out]))

(defn less-than
  [intcode pos [t1 t2 _] [v1 v2 v3] in out]
  [(write-param intcode v3 (if (< (read-param intcode t1 v1)
                                  (read-param intcode t2 v2))
                             1
                             0))
   (+ 4 pos)
   in
   out])

(defn equals
  [intcode pos [t1 t2 _] [v1 v2 v3] in out]
  [(write-param intcode v3 (if (= (read-param intcode t1 v1)
                                  (read-param intcode t2 v2))
                             1
                             0))
   (+ 4 pos)
   in
   out])

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
  [intcode pos in out]
  (let [instruction (parse-instruction (nth intcode pos))
        {:keys [op name size param-types]} instruction
        args (subvec intcode (inc pos) (+ pos size))]
    ; (do
    ;   (println pos name param-types args in out)
    ;   (op intcode pos param-types args in out))
    (op intcode pos param-types args in out)))

(defn intcode-ex
  [intcode in-args]
  (loop [newintcode intcode pos 0 in in-args out []]
    (if (= 99 (nth newintcode pos))
      out
      (let [[nextcode nextpos nextin nextout] (apply-op newintcode pos in out)]
        (recur nextcode nextpos nextin nextout)))))
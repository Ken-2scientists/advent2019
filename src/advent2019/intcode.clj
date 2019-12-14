(ns advent2019.intcode
  (:require [clojure.string :as str]
            [manifold.stream :as s]))

(defn read-param
  [intcode base type value]
  (case type
    :immediate value
    :relative-base (nth intcode (+ base value))
    :position (nth intcode value)))

(defn write-param
  [intcode base type pos value]
  (let [location (case type
                   :relative-base (+ base pos)
                   :position pos)]
    (assoc intcode location value)))

(defn add
  [intcode pos base [t1 t2 t3] [v1 v2 v3] _ _]
  [(write-param intcode base t3 v3 (+ (read-param intcode base t1 v1)
                                      (read-param intcode base t2 v2)))
   (+ 4 pos)
   base])

(defn multiply
  [intcode pos base [t1 t2 t3] [v1 v2 v3] _ _]
  [(write-param intcode base t3 v3 (* (read-param intcode base t1 v1)
                                      (read-param intcode base t2 v2)))
   (+ 4 pos)
   base])

(defn input
  [intcode pos base [t1] [v1] in _]
  [(write-param intcode base t1 v1 @(s/take! in))
   (+ 2 pos)
   base])

(defn output
  [intcode pos base [t1] [v1] _ out]
  (s/put! out (read-param intcode base t1 v1))
  [intcode (+ 2 pos) base])

(defn jump-if-true
  [intcode pos base [t1 t2] [v1 v2] _ _]
  (let [nextpos (if (zero? (read-param intcode base t1 v1))
                  (+ 3 pos)
                  (read-param intcode base t2 v2))]
    [intcode nextpos base]))

(defn jump-if-false
  [intcode pos base [t1 t2] [v1 v2] _ _]
  (let [nextpos (if (zero? (read-param intcode base t1 v1))
                  (read-param intcode base t2 v2)
                  (+ 3 pos))]
    [intcode nextpos base]))

(defn less-than
  [intcode pos base [t1 t2 t3] [v1 v2 v3] _ _]
  [(write-param intcode base t3 v3 (if (< (read-param intcode base t1 v1)
                                          (read-param intcode base t2 v2))
                                     1
                                     0))
   (+ 4 pos)
   base])

(defn equals
  [intcode pos base [t1 t2 t3] [v1 v2 v3] _ _]
  [(write-param intcode base t3 v3 (if (= (read-param intcode base t1 v1)
                                          (read-param intcode base t2 v2))
                                     1
                                     0))
   (+ 4 pos)
   base])

(defn offset
  [intcode pos base [t1] [v1] _ _]
  [intcode (+ 2 pos) (+ base (read-param intcode base t1 v1))])

(defn stop
  [_ _ _ _ _ _ out]
  (s/close! out))

(def ops {1 {:name :add :op add :param-count 3 :size 4}
          2 {:name :multiply :op multiply :param-count 3 :size 4}
          3 {:name :input :op input :param-count 1 :size 2}
          4 {:name :output :op output :param-count 1 :size 2}
          5 {:name :jump-if-true :op jump-if-true :param-count 2 :size 3}
          6 {:name :jump-if-false :op jump-if-false :param-count 2 :size 3}
          7 {:name :less-than :op less-than :param-count 3 :size 4}
          8 {:name :equals :op equals :param-count 3 :size 4}
          9 {:name :relative-base-offset :op offset :param-count 1 :size 2}
          99 {:name :stop :param-count 0 :size 1}})

(defn param-type
  [param-str]
  (case param-str
    \1 :immediate
    \2 :relative-base
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
  [intcode pos base in out]
  (let [instruction (parse-instruction (nth intcode pos))
        {:keys [op size param-types]} instruction
        args (subvec intcode (inc pos) (+ pos size))]
    (op intcode pos base param-types args in out)))

(defn intcode-ex-async
  [intcode in out]
  (loop [newintcode (into intcode (repeat 1000 0)) pos 0 base 0]
    (if (= 99 (nth newintcode pos))
      [newintcode pos base out]
      (let [[nextcode nextpos nextbase] (apply-op newintcode pos base in out)]
        (recur nextcode nextpos nextbase)))))

(defn intcode-ex
  [intcode inputs]
  (let [in (s/->source inputs)
        out (s/stream)]
    (intcode-ex-async intcode in out)))

(defn read-output
  "Read from the output stream. Args are [intcode pos base out]"
  [[_ _ _ out]]
  (s/stream->seq out 100))
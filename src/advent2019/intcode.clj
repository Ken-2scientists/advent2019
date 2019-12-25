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
  [{:keys [intcode base pos]
    [t1 t2 t3] :param-types
    [v1 v2 v3] :args
    :as state}]
  (assoc state
         :intcode (write-param intcode base t3 v3 (+ (read-param intcode base t1 v1)
                                                     (read-param intcode base t2 v2)))
         :pos (+ 4 pos)))

(defn multiply
  [{:keys [intcode base pos]
    [t1 t2 t3] :param-types
    [v1 v2 v3] :args
    :as state}]
  (assoc state
         :intcode (write-param intcode base t3 v3 (* (read-param intcode base t1 v1)
                                                     (read-param intcode base t2 v2)))
         :pos (+ 4 pos)))

(defn input
  [{:keys [intcode base pos in]
    [t1] :param-types
    [v1] :args
    :as state}]
  (assoc state
         :intcode (write-param intcode base t1 v1 @(s/take! in))
         :pos (+ 2 pos)))

(defn output
  [{:keys [intcode base pos out]
    [t1] :param-types
    [v1] :args
    :as state}]
  (s/put! out (read-param intcode base t1 v1))
  (assoc state :pos (+ 2 pos)))

(defn jump-if-true
  [{:keys [intcode base pos]
    [t1 t2] :param-types
    [v1 v2] :args
    :as state}]
  (let [nextpos (if (zero? (read-param intcode base t1 v1))
                  (+ 3 pos)
                  (read-param intcode base t2 v2))]
    (assoc state :pos nextpos)))

(defn jump-if-false
  [{:keys [intcode base pos]
    [t1 t2] :param-types
    [v1 v2] :args
    :as state}]
  (let [nextpos (if (zero? (read-param intcode base t1 v1))
                  (read-param intcode base t2 v2)
                  (+ 3 pos))]
    (assoc state :pos nextpos)))

(defn less-than
  [{:keys [intcode base pos]
    [t1 t2 t3] :param-types
    [v1 v2 v3] :args
    :as state}]
  (assoc state
         :intcode (write-param intcode base t3 v3 (if (< (read-param intcode base t1 v1)
                                                         (read-param intcode base t2 v2))
                                                    1
                                                    0))
         :pos (+ 4 pos)))

(defn equals
  [{:keys [intcode base pos]
    [t1 t2 t3] :param-types
    [v1 v2 v3] :args
    :as state}]
  (assoc state
         :intcode (write-param intcode base t3 v3 (if (= (read-param intcode base t1 v1)
                                                         (read-param intcode base t2 v2))
                                                    1
                                                    0))
         :pos (+ 4 pos)))

(defn offset
  [{:keys [intcode base pos]
    [t1] :param-types
    [v1] :args
    :as state}]
  (assoc state :pos (+ 2 pos) :base (+ base (read-param intcode base t1 v1))))

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
  [{:keys [intcode pos] :as state}]
  (let [instruction (parse-instruction (nth intcode pos))
        {:keys [op size param-types]} instruction
        args (subvec intcode (inc pos) (+ pos size))]
    (op (assoc state :param-types param-types :args args))))

(defn intcode-ex-async
  [intcode in out]
  (loop [state {:intcode (into intcode (repeat 20000 0))
                :pos 0
                :base 0
                :in in
                :out out}]
    (if (= 99 (nth (:intcode state) (:pos state)))
      state
      (recur (apply-op state)))))

(defn intcode-ex
  [intcode inputs]
  (let [in (s/->source inputs)
        out (s/stream)]
    (intcode-ex-async intcode in out)))

(defn read-output
  [{:keys [out]}]
  (s/stream->seq out 25))

(defn cmds->ascii
  [cmds]
  (map (comp int char) (str (str/join "\n" cmds) "\n")))

(defn send-ascii-cmd
  [in cmd]
  (s/put-all! in (map (comp int char) (str cmd "\n"))))

(defn read-ascii-output
  [ascii]
  (str/join (map char ascii)))

(defn interactive-asciicode
  [intcode starter-cmds]
  (let [in (s/stream)
        out (s/stream)
        program (future (intcode-ex-async intcode in out))]
    (when (> (count starter-cmds) 0)
      (s/put-all! in (map (comp int char) (str (str/join "\n" starter-cmds) "\n"))))
    (while (not (realized? program))
      (println (read-ascii-output (s/stream->seq out 100)))
      (send-ascii-cmd in (read-line)))))
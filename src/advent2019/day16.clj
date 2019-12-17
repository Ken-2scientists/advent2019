(ns advent2019.day16
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

(defn nums->str
  [nums]
  (str/join nums))

(defn str->nums
  [s]
  (map (comp read-string str) s))

(def day16-input
  (str->nums (first (u/puzzle-input "day16-input.txt"))))

(def pattern [0 1 0 -1])

(defn digit-calc
  [num pattern]
  (mod (Math/abs (reduce + (map * num pattern))) 10))

(defn patterns
  [size]
  (for [x (range size)]
    (rest (cycle (mapcat (partial repeat (inc x)) pattern)))))

(def patterns-memo (memoize patterns))

(defn phase
  [nums]
  (let [patterns (patterns-memo (count nums))]
    (map (partial digit-calc nums) patterns)))

(defn phase2
  [patterns nums]
  (map (partial digit-calc nums) patterns))

(defn run-phases
  [nums phases]
  (let [size (count nums)
        patterns (patterns size)
        phase-fn (partial phase2 patterns)]
    (loop [signal nums cnt 0]
      (if (= cnt phases)
        signal
        (recur (phase-fn signal) (inc cnt))))))


(defn day16-part1-soln
  []
  (nums->str (take 8 (nth (iterate phase day16-input) 100))))



(defn real-signal
  [nums phases]
  (let [size (count nums)
        signal (take (* size 10000) (cycle nums))
        patterns (patterns (* size 10000))
        phase-fn (partial phase2 patterns)
        offset (read-string (nums->str (take 7 nums)))
        answer (nth (iterate phase-fn signal) phases)]
    (take 8 (drop offset answer))))


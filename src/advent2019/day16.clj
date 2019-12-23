(ns advent2019.day16
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

(defn nums->str
  [nums]
  (str/join nums))

(defn str->nums
  [s]
  (mapv (comp read-string str) s))

(def day16-input
  (str->nums (first (u/puzzle-input "day16-input.txt"))))

(defn selector-lookup
  [size idx]
  (let [adds (for [x (range idx size (* (inc idx) 4))] [x (min size (+ x idx 1))])
        subs (for [x (range (dec (* 3 (inc idx))) size (* (inc idx) 4))] [x (min size (+ x idx 1))])]
    {:add-indices adds
     :sub-indices subs}))

; (defn selector-lookups
;   [size]
;   (map (partial selector-lookup size) (range size)))

(defn selector
  [size idx coll]
  (let [{:keys [add-indices sub-indices]} (selector-lookup size idx)]
    {:adds (vec (mapcat #(apply subvec coll %) add-indices))
     :subs (vec (mapcat #(apply subvec coll %) sub-indices))}))

(defn do-calc
  [{:keys [adds subs]}]
  (mod (Math/abs (- (reduce + adds)
                    (reduce + subs))) 10))

; (defn lookup-and-compute
;   [{:keys [add-indices sub-indices]} num]
;   (let [adds (vec (mapcat #(apply subvec num %) add-indices))
;         subs (vec (mapcat #(apply subvec num %) sub-indices))]
;     (mod (Math/abs (- (reduce + adds)
;                       (reduce + subs))) 10)))

(defn digit-calc
  [size num]
  (let [selections (pmap #(selector size % num) (range size))]
    (mapv do-calc selections)))

(defn phase
  [num]
  (let [size (count num)]
    (digit-calc size num)))

(defn run-phases
  [nums phases]
  (let [size (count nums)
        stepper (partial digit-calc size)]
    (first (drop phases (iterate stepper nums)))))

(defn day16-part1-soln
  []
  (nums->str (take 8 (run-phases day16-input 100))))


; (defn real-signal
;   [nums phases]
;   (let [size (count nums)
;         signal (take (* size 10000) (cycle nums))
;         patterns (patterns (* size 10000))
;         phase-fn (partial phase2 patterns)
;         offset (read-string (nums->str (take 7 nums)))
;         answer (nth (iterate phase-fn signal) phases)]
;     (take 8 (drop offset answer))))


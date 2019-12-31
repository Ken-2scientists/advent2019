(ns advent2019.day22
  (:require [clojure.string :as str]
            [advent2019.lib.math :as math]
            [advent2019.lib.utils :as u]))

(defn parse-line
  [line]
  (mapv read-string
        (-> line
            (str/replace #"deal with increment" ":increment")
            (str/replace #"deal into new stack" ":deal")
            (str/replace #"cut" ":cut")
            (str/split #" "))))

(def day22-input
  (map parse-line (u/puzzle-input "day22-input.txt")))

(defn deal-op
  "A deal into new stack is a mod-multiply by -1 and a mod-add of -1"
  []
  [-1 -1])

(defn cut-op
  "A cut is mod-multiply by 1 (no-op) and a mod-add of the negative of the cut size"
  [arg]
  [1 (- arg)])

(defn inc-op
  "A deal with increment is a mod-multiply by the increment size and mod-add of 0 (no-op)"
  [arg]
  [arg 0])

(defn instruction->op
  [[cmd arg]]
  (case cmd
    :deal (deal-op)
    :cut (cut-op arg)
    :increment (inc-op arg)))

(defn reduced-ops
  [modulus steps]
  (reduce (partial math/mod-linear-comp modulus) (reverse (map instruction->op steps))))

(defn apply-op
  [m [a b] number]
  (mod (+' b (*' a number)) m))

(defn shuffle-deck
  ([size steps]
   (shuffle-deck size steps (range size)))
  ([size steps deck]
   (let [op (reduced-ops size steps)
         indices (zipmap (map (partial apply-op size op) (range size)) (range size))
         lookup (vec deck)]
     (map lookup (map indices (range size))))))

(defn day22-part1-soln
  []
  (u/index-of 2019 (shuffle-deck 10007 day22-input)))

(defn card-after-multiple-shuffles
  [size steps times position]
  (let [pow-op (->> (reduced-ops size steps)
                    (math/mod-linear-inverse size)
                    (math/mod-linear-pow size times))]
    (apply-op size pow-op position)))

(def card-count
  "One hundred nineteen trillion, three hundred fifteen billion, seven hundred seventeem million, 
   five hundred forteen thousan, and forty-seven"
  119315717514047)

(def shuffle-count
  "One hundred one trillion, seven hundred forty-one billion, five hundred eighty-two million,
   seventy-six thousand, six hundred sixty-one"
  101741582076661)

(defn day22-part2-soln
  []
  (card-after-multiple-shuffles card-count day22-input shuffle-count 2020))


(ns advent2019.day22
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

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


(defn deal
  [stack]
  (reverse stack))

(defn cut
  [stack arg size]
  (if (pos? arg)
    (take size (concat (drop arg stack) stack))
    (take size (concat (drop (+ size arg) stack) stack))))

(defn increment
  [stack arg size]
  (let [indices (zipmap (map #(mod (* arg %) size) (range size)) (range size))
        lookup (vec stack)]
    (map lookup (map indices (range size)))))

(defn do-step
  [size stack [cmd arg]]
  (case cmd
    :deal (deal stack)
    :cut (cut stack arg size)
    :increment (increment stack arg size)))

(defn shuffle-deck
  ([size steps]
   (shuffle-deck size steps (range size)))
  ([size steps deck]
   (reduce (partial do-step size) deck steps)))

(defn day22-part1-soln
  []
  (u/index-of 2019 (shuffle-deck 10007 day22-input)))

(defn card-after-multiple-shuffles
  [size steps times position]
  (let [one-shuffle (zipmap (range size) (shuffle-deck size steps))]
    (first (drop times (iterate one-shuffle position)))))

(def card-count 119315717514047)
(def shuffle-count 101741582076661)

(defn day22-part2-soln
  []
  (card-after-multiple-shuffles card-count day22-input shuffle-count 2020))


(ns advent2019.day06
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

(def day06-input
  (->> (u/load-file "day06-input.txt")
       (map #(str/split % #"\)"))
       (mapcat reverse)
       (apply hash-map)))

(def sample-data
  {"B" "COM"
   "C" "B"
   "D" "C"
   "E" "D"
   "F" "E"
   "G" "B"
   "H" "G"
   "I" "D"
   "J" "E"
   "K" "J"
   "L" "K"})

(defn orbit-chain
  [orbits obj]
  (loop [chain [obj] nextobj obj]
    (let [parent (get orbits nextobj)]
      (if (nil? parent)
        chain
        (recur (conj chain parent) parent)))))

(defn orbit-length
  [chain]
  (dec (count chain)))

(defn orbit-count
  [orbits]
  (->> orbits
       keys
       (map (partial orbit-chain orbits))
       (map orbit-length)
       (reduce +)))

(def sample-data2
  {"B" "COM"
   "C" "B"
   "D" "C"
   "E" "D"
   "F" "E"
   "G" "B"
   "H" "G"
   "I" "D"
   "J" "E"
   "K" "J"
   "L" "K"
   "YOU" "K"
   "SAN" "I"})

(defn first-common-ancestor
  [orbits objA objB]
  (let [chainA (orbit-chain orbits objA)
        chainB (orbit-chain orbits objB)
        common (map vector (reverse chainA) (reverse chainB))
        first-delta (first (filter (fn [[a b]] (not= a b)) common))]
    (get orbits (first first-delta))))

(defn distance-to-ancestor
  [orbits des anc]
  (let [chain (orbit-chain orbits des)]
    (count (take-while (partial not= anc) chain))))

(defn orbit-transfers
  [orbits objA objB]
  (let [ancestor (first-common-ancestor orbits objA objB)]
    (- (+ (distance-to-ancestor orbits objA ancestor)
          (distance-to-ancestor orbits objB ancestor))
       2)))

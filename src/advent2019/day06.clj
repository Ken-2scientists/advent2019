(ns advent2019.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def day06-input
  (->> "day06-input.txt"
       io/resource
       io/reader
       line-seq
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

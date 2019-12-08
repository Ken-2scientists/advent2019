(ns advent2019.day08
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

(def day08-input
  (->> (u/puzzle-input "day08-input.txt")
       first
       (map (comp read-string str))))

(defn day08-part1-soln
  []
  (let [layers (partition (* 25 6) day08-input)
        layer-freqs (map frequencies layers)
        min-zero-layer (apply min-key #(get % 0) layer-freqs)]
    (* (get min-zero-layer 1) (get min-zero-layer 2))))

(defn final-pixel
  [pixels]
  (first (filter (partial not= 2) pixels)))

(defn decode-image
  [space-image width height]
  (->> space-image
       (partition (* width height))
       (apply (partial map list))
       (map final-pixel)))

(defn pprint-image
  [image width]
  (let [legible-pixels (replace {0 " " 1 "*"} image)]
    (doseq [line (partition width legible-pixels)]
      (println (str/join line)))))

(defn day08-part2-soln
  []
  (pprint-image (decode-image day08-input 25 6) 25))
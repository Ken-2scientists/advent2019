(ns advent2019.day12
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

(defn parse-coord
  [s]
  (->> (-> s
           (str/replace #"[^\d\-]" " ")
           str/trim
           (str/split #"\s+"))
       (map read-string)))

(def day12-input
  (map parse-coord (u/puzzle-input "day12-input.txt")))

(defn rotations
  [coll]
  (take (count coll) (partition (count coll) 1 (cycle coll))))

(defn pos-compare
  [pos1 pos2]
  (cond
    (< pos1 pos2) 1
    (> pos1 pos2) -1
    :else 0))

(defn velocity-component
  [moon1 moon2]
  (map pos-compare moon1 moon2))

(defn velocity
  [moons]
  (let [velocity-changes (map (partial velocity-component (first moons)) (rest moons))]
    (map #(reduce + %) (apply mapv vector velocity-changes))))

(defn velocities
  [moons]
  (let [perms (rotations moons)]
    (map velocity perms)))

(defn add-deltas
  [old changes]
  (map #(map + %1 %2) old changes))

(defn next-step
  [[moons vels]]
  (let [new-vels (add-deltas vels (velocities moons))
        new-moons (add-deltas moons new-vels)]
    [new-moons new-vels]))

(def initial-velocity
  (repeat 4 (repeat 3 0)))

(defn simulate
  [moons]
  (iterate next-step [moons initial-velocity]))

(defn energy
  [moon]
  (reduce + (map #(Math/abs %) moon)))

(defn total-energy
  [[moons vels]]
  (let [potential-energies (map energy moons)
        kinetic-energies (map energy vels)]
    (reduce + (map * potential-energies kinetic-energies))))

(defn day12-part1-soln
  []
  (total-energy (nth (simulate day12-input) 1000)))
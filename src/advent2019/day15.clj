(ns advent2019.day15
  (:require [manifold.stream :as s]
            [manifold.deferred :as d]
            [advent2019.intcode :as intcode]
            [advent2019.maze :as maze]
            [advent2019.utils :as u]))

(def day15-input (u/puzzle-input-vec "day15-input.txt"))

(def dir->code
  {:north 1
   :south 2
   :west 3
   :east 4})

(def status
  {0 :wall
   1 :open
   2 :oxygen})

(defn tried-position
  [[x y] direction]
  (case direction
    :north [x (dec y)]
    :south [x (inc y)]
    :east [(inc x) y]
    :west [(dec x) y]))

(defn update-mazemap
  [{:keys [maze position direction] :as state} result]
  (let [tested-pos (tried-position position direction)
        new-maze (assoc maze tested-pos result)]
    (case result
      :wall (merge state {:maze new-maze} {:direction (maze/next-direction direction :right)})
      :open (merge state {:maze new-maze} {:position tested-pos})
      :oxygen (merge state {:maze new-maze} {:position tested-pos}))))

(defn droid-step
  [in out {:keys [maze position direction] :as state}]
  (let [dir (maze/maze-mapper maze position direction)
        _ (s/put! in (dir->code dir))
        result (status @(s/try-take! out 20))]
    (update-mazemap (assoc state :direction dir) result)))

(defn map-maze
  [intcode]
  (let [in (s/stream)
        out (s/stream)
        stepper (partial droid-step in out)
        _ (d/future (intcode/intcode-ex-async intcode in out))]
    (loop [state {:maze {[0,0] :open} :position [0,0] :direction :north}]
      (if (and (> (count (state :maze)) 100) (= [0 0] (state :position)))
        state
        (recur (stepper state))))))

(defn day15-part1-soln
  []
  (let [maze (:maze (map-maze day15-input))
        path-to-end (maze/find-path maze [0 0])]
    (count path-to-end)))

(defn day15-part2-soln
  []
  (let [maze (:maze (map-maze day15-input))
        oxygen (ffirst (filter #(= :oxygen (val %)) maze))]
    (maze/flood-fill maze oxygen)))
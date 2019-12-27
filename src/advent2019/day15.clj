(ns advent2019.day15
  (:require [lanterna.screen :as scr]
            [manifold.stream :as s]
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

(defn all-open
  [maze]
  (map first (filter #(not= :wall (val %)) maze)))

(defn open-neighbors
  [maze pos]
  (all-open (maze/better-neighbors maze pos)))

(defn distance
  [_ _]
  1)

(defn find-path
  [maze start finish]
  (maze/dijkstra maze (count (all-open maze)) open-neighbors distance start finish))

(defn day15-part1-soln
  []
  (let [maze ((map-maze day15-input) :maze)
        start [0 0]
        finish (maze/find-target maze :oxygen)
        simplified-maze (maze/relabel-dead-paths maze merge all-open open-neighbors #{start finish} :wall)
        path-to-end (find-path simplified-maze start finish)]
    (count path-to-end)))

(defn day15-part2-soln
  []
  (let [maze (:maze (map-maze day15-input))
        oxygen (maze/find-target maze :oxygen)]
    (maze/flood-fill maze oxygen)))

(defn val->str
  [val]
  (case val
    :open "."
    :wall "#"
    :oxygen "O"))

(defn show-maze
  [maze]
  (let [screen (scr/get-screen :swing {:rows 50 :cols 80})]
    (scr/start screen)
    (doseq [[[x y] val] maze]
      (scr/put-string screen (+ x 40) (+ y 21) (val->str val)))
    (scr/redraw screen)))
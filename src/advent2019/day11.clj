(ns advent2019.day11
  (:require [manifold.stream :as s]
            [manifold.deferred :as d]
            [advent2019.lib.intcode :as intcode]
            [advent2019.lib.utils :as u]))

(def day11-input (u/puzzle-input-vec "day11-input.txt"))

(defn turn-left
  [{:keys [direction] [x y] :position}]
  (case direction
    :up {:position [(dec x) y] :direction :left}
    :left {:position [x (dec y)] :direction :down}
    :down {:position [(inc x) y] :direction :right}
    :right {:position [x (inc y)] :direction :up}))

(defn turn-right
  [{:keys [direction] [x y] :position}]
  (case direction
    :up {:position [(inc x) y] :direction :right}
    :right {:position [x (dec y)] :direction :down}
    :down {:position [(dec x) y] :direction :left}
    :left {:position [x (inc y)] :direction :up}))

(defn move-and-paint
  [{:keys [hull position] :as state}
   paint
   turn]
  (let [newhull (assoc hull position paint)]
    (merge state
           {:hull newhull}
           (case turn
             0 (turn-left state)
             1 (turn-right state)))))

(defn robot-step
  [in out {:keys [hull position] :as state}]
  (let [_ (s/put! in (get hull position 0))
        paint @(s/try-take! out 100)
        turn @(s/try-take! out 100)]
    (if (and paint turn)
      (move-and-paint state paint turn)
      state)))

(defn paint-bot
  [intcode start]
  (let [in (s/stream)
        out (s/stream)
        stepper (partial robot-step in out)
        program (d/future (intcode/intcode-ex-async intcode in out))]
    (loop [state {:hull {[0,0] start} :position [0,0] :direction :up}]
      (if (realized? program)
        state
        (recur (stepper state))))))

(defn day11-part1-soln
  []
  (count (keys (get (paint-bot day11-input 0) :hull))))

(defn pattern->pixels
  [pattern]
  (let [xs (map first (keys pattern))
        ys (map second (keys pattern))
        minx (apply min xs)
        maxx (apply max xs)
        miny (apply min ys)
        maxy (apply max ys)
        pixels (for [y (range maxy (dec miny) -1)
                     x (range minx (inc maxx))]
                 (get pattern [x y] 0))]
    [pixels (inc (- maxx minx))]))

(defn day11-part2-soln
  []
  (let [pattern (get (paint-bot day11-input 1) :hull)
        [image width] (pattern->pixels pattern)]
    (u/pprint-image image width)))
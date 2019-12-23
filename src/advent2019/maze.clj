(ns advent2019.maze
  (:require [lanterna.screen :as scr]
            [advent2019.utils :as u]))

(defn relative-direction
  [direction]
  (case direction
    :north {:forward :north :left :west :backward :south :right :east}
    :west {:forward :west :left :south :backward :east :right :north}
    :south {:forward :south :left :east :backward :north :right :west}
    :east {:forward :east :left :north :backward :west :right :south}))

(defn next-direction
  [direction turn]
  ((relative-direction direction) turn))

(defn one-step
  [[x y] direction]
  (case direction
    :north [x (dec y)]
    :south [x (inc y)]
    :east [(inc x) y]
    :west [(dec x) y]))

(defn better-neighbors
  [maze [x y]]
  (let [coords [[x (dec y)] [(dec x) y] [x (inc y)] [(inc x) y]]
        vals (map maze coords)]
    (zipmap coords vals)))

(defn neighbors
  [maze [x y]]
  (let [north (get maze [x (dec y)])
        west (get maze [(dec x) y])
        south (get maze [x (inc y)])
        east (get maze [(inc x) y])]
    [north west south east]))

(defn relative-neighbors
  [maze [x y] direction]
  (let [[north west south east] (neighbors maze [x y])]
    (case direction
      :north {:forward north :left west :backward south :right east}
      :west {:forward west :left south :backward east :right north}
      :south {:forward south :left east :backward north :right west}
      :east {:forward east :left north :backward west :right south})))

(defn follow-left-wall
  [neighbors]
  (case (neighbors :left)
    :open :left
    nil :left
    :wall (if (= :wall (neighbors :forward))
            (if (= :wall (neighbors :right))
              :backward
              :right)
            :forward)))

(defn maze-mapper
  [maze position direction]
  (let [neighbors (relative-neighbors maze position direction)]
    (next-direction direction (follow-left-wall neighbors))))

(defn dead-end?
  [maze [x y]]
  (if (= :open (maze [x y]))
    (let [neighbors (neighbors maze [x y])
          open (u/count-if neighbors #(= :open %))
          walls (u/count-if neighbors #(= :wall %))]
      (and (= open 1) (= walls 3)))
    false))

(defn intersection?
  [maze [x y]]
  (let [neighbors (neighbors maze [x y])
        open (u/count-if neighbors #(= :open %))]
    (> open 2)))

(defn finish?
  [maze [x y]]
  (= :oxygen (maze [x y])))

(defn direction-at-dead-end
  [maze [x y]]
  (let [options (zipmap [:north :west :south :east] (neighbors maze [x y]))]
    (ffirst (filter #(not= :wall (val %)) options))))

(defn next-pos-and-dir
  [maze [x y] direction]
  (let [neighbors (relative-neighbors maze [x y] direction)
        turn (ffirst (filter #(not= :wall (val %)) (dissoc neighbors :backward)))
        new-dir (next-direction direction turn)]
    [(one-step [x y] new-dir) new-dir]))

(defn path-to-condition
  "Find the deterministic path from the starting point until pred is true"
  [maze pred [x y]]
  (loop [points [[x y]] pos [x y] dir (direction-at-dead-end maze [x y])]
    (if (pred maze pos)
      (butlast points)
      (let [[new-pos new-dir] (next-pos-and-dir maze pos dir)]
        (recur (conj points new-pos) new-pos new-dir)))))

(defn eliminate-dead-paths
  [maze start]
  (loop [newmaze maze]
    (let [dead-end-pred (every-pred (partial dead-end? newmaze) (partial not= start))
          dead-ends (filter dead-end-pred (keys newmaze))]
      (if (= 0 (count dead-ends))
        newmaze
        (recur (merge newmaze
                      (zipmap (mapcat (partial path-to-condition newmaze intersection?) dead-ends) (repeat :wall))))))))

(defn find-path
  [maze start]
  (let [simplified-maze (eliminate-dead-paths maze start)]
    (path-to-condition simplified-maze finish? [0 0])))

(defn spread-to-adjacent
  [maze [x y]]
  (let [thens (better-neighbors maze [x y])
        to-add (filter #(= :open (val %)) thens)]
    (keys to-add)))

(defn flood-fill
  [maze start]
  (loop [newmaze maze last-added [start] count 0]
    (if (= 0 (u/count-if newmaze #(= :open (val %))))
      count
      (let [changes (mapcat (partial spread-to-adjacent newmaze) last-added)
            updates (merge newmaze (zipmap changes (repeat :oxygen)))]
        (recur updates changes (inc count))))))

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
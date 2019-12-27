(ns advent2019.maze
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]
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

(defn adj-coords
  "Coordinates of four adjacent points, always in the order n w s e"
  [[x y]]
  [[x (dec y)] [(dec x) y] [x (inc y)] [(inc x) y]])

(defn better-neighbors
  [maze pos]
  (let [coords (adj-coords pos)
        vals (map maze coords)]
    (zipmap coords vals)))

(defn neighbors
  [maze pos]
  (mapv maze (adj-coords pos)))

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

; (defn dead-end?
;   [maze [x y]]
;   (if (= :open (maze [x y]))
;     (let [neighbors (neighbors maze [x y])
;           open (u/count-if neighbors #(= :open %))
;           walls (u/count-if neighbors #(= :wall %))]
;       (and (= open 1) (= walls 3)))
;     false))

; (defn intersection?
;   [maze [x y]]
;   (let [neighbors (neighbors maze [x y])
;         open (u/count-if neighbors #(= :open %))]
;     (> open 2)))

; (defn finish?
;   [maze [x y]]
;   (= :oxygen (maze [x y])))

; (defn direction-at-dead-end
;   [maze [x y]]
;   (let [options (zipmap [:north :west :south :east] (neighbors maze [x y]))]
;     (ffirst (filter #(not= :wall (val %)) options))))

; (defn next-pos-and-dir
;   [maze [x y] direction]
;   (let [neighbors (relative-neighbors maze [x y] direction)
;         turn (ffirst (filter #(not= :wall (val %)) (dissoc neighbors :backward)))
;         new-dir (next-direction direction turn)]
;     [(one-step [x y] new-dir) new-dir]))

; (defn path-to-condition
;   "Find the deterministic path from the starting point until pred is true"
;   [maze pred [x y]]
;   (loop [points [[x y]] pos [x y] dir (direction-at-dead-end maze [x y])]
;     (if (pred maze pos)
;       (butlast points)
;       (let [[new-pos new-dir] (next-pos-and-dir maze pos dir)]
;         (recur (conj points new-pos) new-pos new-dir)))))

; (defn eliminate-dead-paths
;   [maze start]
;   (loop [newmaze maze]
;     (let [dead-end-pred (every-pred (partial dead-end? newmaze) (partial not= start))
;           dead-ends (filter dead-end-pred (keys newmaze))]
;       (if (= 0 (count dead-ends))
;         newmaze
;         (recur (merge newmaze
;                       (zipmap (mapcat (partial path-to-condition newmaze intersection?) dead-ends) (repeat :wall))))))))

; (defn find-path
;   [maze start]
;   (let [simplified-maze (eliminate-dead-paths maze start)]
;     (path-to-condition simplified-maze finish? [0 0])))

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

(defn entries-in-set
  [s m]
  (filter (fn [[k _]] (s k)) m))

(defn entries-not-in-set
  [s m]
  (filter (fn [[k _]] ((complement s) k)) m))

(defn dijkstra-update
  [distance-fn node {:keys [dist prev] :as state} neighbor]
  (let [alt (+ (dist node) (distance-fn node neighbor))]
    (if (or (nil? (dist neighbor)) (< alt (dist neighbor)))
      {:dist (assoc dist neighbor alt) :prev (assoc prev neighbor node)}
      state)))

(defn dijkstra-retrace
  [prev-steps finish]
  (loop [node finish chain []]
    (if (nil? node)
      chain
      (recur (prev-steps node) (conj chain node)))))

(defn dijkstra
  [graph max-search edges-fn distance-fn start finish]
  (let [init-state {:dist (priority-map start 0) :prev {}}]
    (loop [visited #{} node start state init-state]
      (if (or (= max-search (count visited)) (= node finish))
        (drop 1 (reverse (dijkstra-retrace (state :prev) finish)))
        (let [neighbors (filter (complement visited) (edges-fn graph node))
              new-state (reduce (partial dijkstra-update distance-fn node) state neighbors)]
          (recur (conj visited node) (ffirst (entries-not-in-set visited (state :dist))) new-state))))))

(defn find-target
  [maze target]
  (ffirst (filter #(= target (val %)) maze)))

(defn dead-end?
  [graph edges-fn pos]
  (= 1 (count (edges-fn graph pos))))

(defn only-available-path
  "Find the only available path until a choice is presented"
  [graph edges-fn start]
  (loop [visited #{} node start]
    (let [next-neighbors (filter (complement visited) (edges-fn graph node))]
      (if (> (count next-neighbors) 1)
        (seq visited)
        (recur (conj visited node) (first next-neighbors))))))

(defn relabel-dead-paths
  [graph update-fn vertices-fn edges-fn exclude-set label]
  (loop [newgraph graph]
    (let [dead-end-pred (every-pred (partial dead-end? newgraph edges-fn) (complement exclude-set))
          dead-ends (filter dead-end-pred (vertices-fn newgraph))]
      (if (= 0 (count dead-ends))
        newgraph
        (recur (update-fn newgraph
                          (zipmap (mapcat (partial only-available-path newgraph edges-fn) dead-ends)
                                  (repeat label))))))))

(defn printable-maze
  [charmap {:keys [maze] {:keys [width height]} :dims}]
  (let [char-seqs (partition width (for [y (range height)
                                         x (range width)]
                                     (charmap (maze [x y]))))]
    (str (str/join "\n" (mapv #(apply str %) char-seqs)) "\n")))
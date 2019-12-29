(ns advent2019.lib.maze
  (:require [clojure.string :as str]
            [advent2019.lib.graph :as g :refer [Graph vertices ->MapGraph]]
            [advent2019.lib.utils :as u]))

(def relative-dirs [:forward :left :backward :right])
(def cardinal-dirs [:north :west :south :east])

(defn relative-direction
  [direction]
  (zipmap relative-dirs (u/rotate (u/index-of direction cardinal-dirs) cardinal-dirs)))

(defn next-direction
  [direction turn]
  ((relative-direction direction) turn))

(defn adj-coords
  "Coordinates of four adjacent points, always in the order N W S E"
  [[x y]]
  [[x (dec y)] [(dec x) y] [x (inc y)] [(inc x) y]])

(defn one-step
  [pos direction]
  ((adj-coords pos) (u/index-of direction cardinal-dirs)))

(defn neighbors
  [maze pos]
  (let [coords (adj-coords pos)
        vals (map maze coords)]
    (zipmap coords vals)))

(defn relative-neighbors
  [maze pos direction]
  (let [neighbor-vals (mapv maze (adj-coords pos))]
    (zipmap relative-dirs (u/rotate (u/index-of direction cardinal-dirs) neighbor-vals))))

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

(defn all-open
  [open? maze]
  (map first (filter #(open? (val %)) maze)))

(defrecord Maze [maze open?]
  Graph
  (vertices
    [_]
    (all-open open? maze))

  (edges
    [_ v]
    (all-open open? (neighbors maze v)))

  (distance
    [_ _ _]
    1)

  (without-vertex
    [_ v]
    (->Maze (assoc maze v :wall))))

(defn summarize-path
  [g path]
  [(first path) {(last path) (g/path-distance g path)}])

(defn adjacencies
  [maze]
  (let [leaves (filter (partial g/leaf? maze) (vertices maze))
        junctions (filter (partial g/junction? maze) (vertices maze))
        nodes (concat leaves junctions)]
    (->> (mapcat (partial g/all-paths maze) nodes)
         (map (partial summarize-path maze))
         (group-by first)
         (u/fmap #(apply merge (map second %))))))

(defn Maze->Graph
  [maze]
  (->MapGraph (adjacencies maze)))

(defn spread-to-adjacent
  [maze [x y]]
  (let [thens (neighbors maze [x y])
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

(defn find-target
  [maze target]
  (ffirst (filter #(= target (val %)) maze)))

(defn printable-maze
  [charmap {:keys [maze] {:keys [width height]} :dims}]
  (let [char-seqs (partition width (for [y (range height)
                                         x (range width)]
                                     (charmap (maze [x y]))))]
    (str (str/join "\n" (mapv #(apply str %) char-seqs)) "\n")))
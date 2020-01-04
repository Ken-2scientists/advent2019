(ns advent2019.day18
  (:require [clojure.string :as str]
            [advent2019.lib.ascii :as ascii]
            [advent2019.lib.graph :as g :refer [map->MapGraph
                                                vertices
                                                distance
                                                without-vertex
                                                rewired-without-vertex]]
            [advent2019.lib.maze :as maze :refer [map->Maze]]
            [advent2019.lib.utils :as u]))

(def day18-input (vec (u/puzzle-input "day18-input.txt")))

(defn maze-map
  [char]
  (let [s (str char)]
    (case s
      "." :open
      "#" :wall
      "@" :entrance
      (if (re-find #"[a-z]" (str char))
        [:key s]
        [:door (str/lower-case s)]))))

; TODO - think about when to cull the dead ends from the maze
(defn load-maze
  [maze]
  (let [themaze (ascii/ascii->map maze-map maze)
        entrance (ffirst (filter #(= :entrance (val %)) themaze))
        specials (into {} (filter #(not (keyword? (val %))) themaze))
        keys (u/invert-map (u/fmap second (into {} (filter #(= :key (first (val %))) specials))))
        doors (u/invert-map (u/fmap second (into {} (filter #(= :door (first (val %))) specials))))
        nodes (concat [entrance] (vals keys) (vals doors))]
    (map->Maze
     {:maze themaze
      :open? (partial not= :wall)
      :entrance entrance
      :keys keys
      :doors doors
      :nodes nodes})))

;; Consider merging this back into the graph namespace

(defn adjacencies
  [{:keys [nodes] :as maze}]
  (let [leaves    (filter (partial g/leaf? maze) (vertices maze))
        junctions (filter (partial g/junction? maze) (vertices maze))
        vs (concat leaves junctions nodes)]
    (->> (mapcat #(g/all-paths maze % :excludes nodes) vs)
         (map (partial g/summarize-path maze))
         (group-by first)
         (u/fmap #(apply merge (map second %))))))

(defn to-graph
  [maze]
  (map->MapGraph (merge maze {:graph (adjacencies maze)})))

(defn load-graph
  [maze]
  (to-graph (load-maze maze)))

(defn terminal-keys
  [{:keys [keys] :as graph}]
  (let [key-locs (vals keys)]
    (filter (partial g/leaf? graph) key-locs)))

(defn route-scout
  [{:keys [entrance nodes maze] :as graph} key-loc]
  (let [path (g/dijkstra graph entrance key-loc)
        distance (g/path-distance graph path)
        objects (map maze (filter (disj (set nodes) entrance key-loc) path))]
    {key-loc {:route path
              :dist distance
              :objects objects}}))

(defn key-routes
  [graph]
  (let [t-keys (terminal-keys graph)]
    (into {} (map (partial route-scout graph) t-keys))))

;; TODO --- add a splice (wrong name?) function to the Graph protocol to remove a junction from the graph, but updating the
;; neighbors so they are now directly connected with the correct distances.

;IDEA 2020-01-01:
; Identify the terminal keys (keys that are at leaf node on the graph) --- make sure the graph has been pruned first
; Compute the path from those keys to the entrance.
;;; Determine what other objects (keys doors) are along that path from the terminal key to the entrance
;;; Estimate the total distance as the sum of the round-trip distances from the entrance to each terminal key
;;; Keep track of the pre-requisites to get to a terminal key (i.e. what other keys are necessary first)
;;; When you have a choice in where to go, update the estimates according to how the total distance would end up changing


;;; Sketch of an algorithm
; Start at entrance.
; what keys are reachable?
   ;;; "reachable" means that there's a path between current position and key in question without passing through a door
   ; if only one key is reachable, move to get that key -> remove the corresponding door from the problem.
   ; if more than one key is reachable, which one will minimize total steps over time?  That's the hard part.

   ; Somehow, make the optimimum choice to go to a key.
   ; recur now, with the newly available information

(defn door?
  [maze pos]
  (let [value (maze pos)]
    (and (not (keyword value)) (= :door (first value)))))

(defn key?
  [maze pos]
  (let [value (maze pos)]
    (and (not (keyword value)) (= :key (first value)))))

(defn door-or-key?
  [maze pos]
  (or (door? maze pos) (key? maze pos)))

(defn terminal-key?
  [maze pos]
  (and (key? maze pos) (g/leaf? maze))
  (let [value (maze pos)]
    (and (not (keyword value)) (= :key (first value)))))

(defn move
  [{:keys [pos steps maze doors] :as state} keypos]
  (println keypos)
  (let [newkey (second (maze keypos))
        door (doors newkey)
        dist (g/shortest-distance state pos keypos)]
    (println keypos newkey door dist)
    (-> state
        (rewired-without-vertex door)
        (assoc-in [:maze keypos] :open)
        ; (rewired-without-vertex keypos)
        (assoc :pos keypos)
        (update :collected-keys conj newkey)
        (assoc :steps (+ steps dist))
        (assoc :laststep dist))))

(defn next-move
  [{:keys [pos steps maze doors graph] :as state}]
  (let [reachable (filter (partial key? maze) (g/reachable state pos (partial door-or-key? maze)))]
    (println reachable)
    (if (= 1 (count reachable))
      (move state (first reachable))
      (if (= 2 (count reachable))
        (move state (first (filter (partial g/leaf? state) reachable)))
        (println "Need to make a decision")))))

(defn shortest-path
  [{:keys [entrance] :as state}]
  (loop [newstate (assoc state
                         :collected-keys []
                         :pos entrance
                         :steps 0
                         :laststep 0
                         :key-routes (key-routes state))]
    (println (select-keys newstate [:graph :steps :laststep :pos :collected-keys]))
    (if (= (count (:collected-keys newstate)) (count (:keys newstate)))
      (:steps newstate)
      (recur (next-move newstate)))))
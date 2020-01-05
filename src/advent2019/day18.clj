(ns advent2019.day18
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.set :as set]
            [clojure.string :as str]
            [advent2019.lib.ascii :as ascii]
            [advent2019.lib.graph :as g :refer [Graph
                                                map->MapGraph
                                                edges
                                                vertices
                                                distance
                                                without-vertex
                                                rewired-without-vertex]]
            [advent2019.lib.maze :as maze :refer [map->Maze]]
            [advent2019.lib.utils :as u]))

(def day18-input (vec (u/puzzle-input "day18-input.txt")))

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
  (and (key? maze pos) (g/leaf? maze pos)))

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

(defn unnecessary-objects
  "If a key is found along the way to a door, then neither that key nor the door need to be tracked"
  [route]
  (->> route
       :objects
       (map second)
       frequencies
       (filter #(> (val %) 1))
       (map first)))

; TODO - make this work
(defn unnecessary-keys
  "If a key doesn't have a corresponding door and isn't a terminal key, it doesn't need to be tracked"
  [{:keys [maze] :as graph}]
  (let [locs (filter (complement (partial g/leaf? graph))
                     (map (:keys graph) (set/difference (set (keys (:keys graph)))
                                                        (set (keys (:doors graph))))))]
    (zipmap locs (map (comp second maze) locs))))

(defn simplify-graph
  [{:keys [doors nodes] :as graph}]
  (let [routes (key-routes graph)
        unneeded-keys (unnecessary-keys graph)
        removals (concat (mapcat unnecessary-objects (vals routes))
                         (vals unneeded-keys))
        locations (concat (mapcat (fn [x] [(doors x) ((:keys graph) x)]) removals)
                          (keys unneeded-keys))
        newgraph (reduce rewired-without-vertex graph locations)]
    (-> newgraph
        (assoc :keys (u/without-keys (:keys newgraph) removals))
        (assoc :doors (u/without-keys doors removals))
        (assoc :nodes (filter (complement (set locations)) nodes)))))

(defn route-needs
  [route]
  (map second (filter #(= :door (first %)) (:objects route))))

(defn needs
  [graph]
  (let [routes (key-routes graph)
        doors (map (:keys graph) (mapcat route-needs (vals routes)))]
    ; (println routes)
    ; (println doors)
    (set (concat (keys routes) doors))))

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

;;; need some notion of picking up a key "on the way" (e.g. key 'e' in d18-s3 on the way to 'g')
(defn move
  [{:keys [pos steps maze doors] :as state} keypos]
  ; (println keypos)
  (let [newkey (second (maze keypos))
        door (doors newkey)
        dist (g/shortest-distance state pos keypos)]
    ; (println keypos newkey door dist)
    (-> state
        (rewired-without-vertex door)
        ; (assoc-in [:maze keypos] :open)
        ; (rewired-without-vertex keypos)
        (assoc :pos keypos)
        (update :collected-keys conj newkey)
        (update :keys dissoc newkey)
        (update :doors dissoc newkey)
        (assoc :steps (+ steps dist))
        (assoc :laststep dist))))

(declare shortest-path-step)
(defn chose-option
  [state options]
  (let [distances (map #(shortest-path-step (move state %)) options)
        min-dist (apply min distances)
        best-index (u/index-of min-dist distances)]
    (nth options best-index)))

(defn next-move
  [{:keys [pos steps maze doors graph] :as state}]
  (let [needs (needs state)
        reachable-keys (set (filter (partial key? maze) (g/reachable state pos (partial door? maze))))
        options (vec (set/intersection needs reachable-keys))]
    ; (println needs reachable-keys options)
    (if (= 1 (count options))
      (move state (first options))
      (move state (chose-option state options)))))

(defn shortest-path-step
  [{:keys [entrance] :as state}]
  (loop [newstate state]
    (println (:collected-keys newstate))
    ; (println (select-keys newstate [:graph :steps :pos :keys :doors :collected-keys]))
    (if (zero? (count (:keys newstate)))
      (:steps newstate)
      (recur (next-move newstate)))))

(defn shortest-path-orig
  [{:keys [entrance] :as state}]
  (shortest-path-step (assoc (simplify-graph state)
                             :collected-keys []
                             :pos entrance
                             :steps 0
                             :laststep 0)))

(defn node-name
  [node]
  (if (= :entrance node)
    "@"
    (second node)))

(defn all-routes-for-node
  [{:keys [maze] :as graph} others node]
  (let [paths (map (partial g/dijkstra graph node) (filter (partial not= node) others))
        dists (map (partial g/path-distance graph) paths)
        needs (map #(map (comp node-name maze) (filter (partial door-or-key? maze) (butlast %))) paths)]
    {(node-name (maze node))
     (zipmap (map (comp node-name maze last) paths)
             (map (fn [x y] {:dist x :needs y}) dists needs))}))

(defn fully-connected-keys
  [graph]
  (let [vertices (concat (vals (:keys graph)) [(:entrance graph)])]
    (apply merge (map (partial all-routes-for-node graph vertices) vertices))))

(defrecord LockedGraph [graph]
  Graph
  (vertices
    [_]
    (keys graph))

  (edges
    [_ v]
    (let [collected-keys (map str v)
          previous-keys (set (map str (drop-last v)))
          vertex (str (last v))
          es (filter #(empty? (set/difference (set (get-in graph [vertex % :needs]))
                                              (set collected-keys)))
                     (keys (graph vertex)))]
      (map #(str/join (concat collected-keys %)) (filter (complement previous-keys) es))))

  (distance
    [_ v1 v2]
    (get-in graph [(str (last v1)) (str (last v2)) :dist])))

(defn locked-dijkstra
  [graph start]
  (let [max-keys (count (vertices graph))
        init-state {:dist (priority-map start 0) :prev {}}]
    (loop [visited #{}
           vertex start
           state init-state]
      (if (= max-keys (count vertex))
        (map str vertex)
        (let [neighbors (filter (complement visited) (edges graph vertex))
              new-state (reduce (partial g/dijkstra-update graph vertex) state neighbors)]
          (recur (conj visited vertex)
                 (ffirst (g/entries-not-in-set visited (state :dist)))
                 new-state))))))

(defn shortest-path
  [input]
  (let [graph (->> input
                   ;(g/pruned)
                   simplify-graph
                   fully-connected-keys
                   ->LockedGraph)
        path (locked-dijkstra graph "@")]
    (println path)
    (g/path-distance graph path)))
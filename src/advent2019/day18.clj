(ns advent2019.day18
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.set :as set]
            [clojure.string :as str]
            [advent2019.lib.ascii :as ascii]
            [advent2019.lib.graph :as g :refer [Graph
                                                ->MapGraph
                                                map->MapGraph
                                                edges
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
      s)))

; TODO - think about when to cull the dead ends from the maze
(defn load-maze
  [maze]
  (let [themaze (ascii/ascii->map maze-map maze)
        entrances (map first (filter #(= :entrance (val %)) themaze))
        specials (into {} (filter #(not (keyword? (val %))) themaze))
        keys (map first (filter #(re-find #"[a-z]" (val %)) specials))
        doors (map first (filter #(re-find #"[A-Z]" (val %)) specials))
        nodes (concat entrances keys doors)]
    (map->Maze
     {:maze themaze
      :open? (partial not= :wall)
      :entrances (zipmap entrances (range))
      :keys (map themaze keys)
      :doors (map themaze doors)
      :nodes nodes})))

(defn unique-label
  [{:keys [maze entrances]} [x y]]
  (let [foo (maze [x y])]
    (if (keyword? foo)
      (if (= :entrance foo)
        (str "@" (entrances [x y]))
        (str "_" x "," y))
      foo)))

(defn summarize-path
  [path]
  [(first path) {(last path) (dec (count path))}])

; Consider merging this back into the graph namespace
(defn adjacencies
  [{:keys [nodes] :as maze}]
  (let [leaves    (filter (partial g/leaf? maze) (vertices maze))
        junctions (filter (partial g/junction? maze) (vertices maze))
        vs (concat leaves junctions nodes)]
    (->> (mapcat #(g/all-paths maze % :excludes nodes) vs)
         (map #(map (partial unique-label maze) %))
         (map summarize-path)
         (group-by first)
         (u/fmap #(apply merge (map second %))))))

(defn load-graph
  [input]
  (let [{:keys [entrances keys doors] :as maze} (load-maze input)
        ents (map #(str "@" %) (vals entrances))
        graph (map->MapGraph {:graph (adjacencies maze)
                              :keys keys
                              :doors doors
                              :entrances ents})
        excludes (set (concat keys doors ents))]
    (g/pruned graph excludes)))

(defn door?
  [{:keys [doors]} vertex]
  ((set doors) vertex))

(defn key?
  [{:keys [keys]} vertex]
  ((set keys) vertex))

(defn terminal-key?
  [graph vertex]
  (and (key? graph vertex) (g/leaf? graph vertex)))

(defn door-or-key?
  [graph vertex]
  (or (door? graph vertex) (key? graph vertex)))

(defn terminal-keys
  [{:keys [keys] :as graph}]
  (filter (partial g/leaf? graph) keys))

; (defn route-scout
;   [{:keys [entrances nodes maze] :as graph} key-loc]
;   (let [path (g/dijkstra graph (first entrances) key-loc)
;         distance (g/path-distance graph path)
;         objects (map maze (filter (disj (set nodes) (first entrances) key-loc) path))]
;     {key-loc {:route path
;               :dist distance
;               :objects objects}}))

; (defn key-routes
;   [graph]
;   (let [t-keys (terminal-keys graph)]
;     (into {} (map (partial route-scout graph) t-keys))))

; (defn unnecessary-objects
;   "If a key is found along the way to a door, then neither that key nor the door need to be tracked"
;   [route]
;   (->> route
;        :objects
;        (map second)
;        frequencies
;        (filter #(> (val %) 1))
;        (map first)))

; (defn unnecessary-keys
;   "If a key doesn't have a corresponding door and isn't a terminal key, it doesn't need to be tracked"
;   [{:keys [maze] :as graph}]
;   (let [locs (filter (complement (partial g/leaf? graph))
;                      (map (:keys graph) (set/difference (set (keys (:keys graph)))
;                                                         (set (keys (:doors graph))))))]
;     (zipmap locs (map (comp second maze) locs))))

; (defn simplify-graph
;   [{:keys [doors nodes] :as graph}]
;   (let [routes (key-routes graph)
;         unneeded-keys (unnecessary-keys graph)
;         removals (concat (mapcat unnecessary-objects (vals routes))
;                          (vals unneeded-keys))
;         locations (concat (mapcat (fn [x] [(doors x) ((:keys graph) x)]) removals)
;                           (keys unneeded-keys))
;         newgraph (reduce rewired-without-vertex graph locations)]
;     (-> newgraph
;         (assoc :keys (u/without-keys (:keys newgraph) removals))
;         (assoc :doors (u/without-keys doors removals))
;         (assoc :nodes (filter (complement (set locations)) nodes)))))

; (defn route-needs
;   [route]
;   (map second (filter #(= :door (first %)) (:objects route))))

; (defn needs
;   [graph]
;   (let [routes (key-routes graph)
;         doors (map (:keys graph) (mapcat route-needs (vals routes)))]
;     ; (println routes)
;     ; (println doors)
;     (set (concat (keys routes) doors))))

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
; (defn move
;   [{:keys [pos steps maze doors] :as state} keypos]
;   ; (println keypos)
;   (let [newkey (second (maze keypos))
;         door (doors newkey)
;         dist (g/shortest-distance state pos keypos)]
;     ; (println keypos newkey door dist)
;     (-> state
;         (rewired-without-vertex door)
;         ; (assoc-in [:maze keypos] :open)
;         ; (rewired-without-vertex keypos)
;         (assoc :pos keypos)
;         (update :collected-keys conj newkey)
;         (update :keys dissoc newkey)
;         (update :doors dissoc newkey)
;         (assoc :steps (+ steps dist))
;         (assoc :laststep dist))))

; (declare shortest-path-step)
; (defn chose-option
;   [state options]
;   (let [distances (map #(shortest-path-step (move state %)) options)
;         min-dist (apply min distances)
;         best-index (u/index-of min-dist distances)]
;     (nth options best-index)))

; (defn next-move
;   [{:keys [pos steps maze doors graph] :as state}]
;   (let [needs (needs state)
;         reachable-keys (set (filter (partial key? maze) (g/reachable state pos (partial door? maze))))
;         options (vec (set/intersection needs reachable-keys))]
;     ; (println needs reachable-keys options)
;     (if (= 1 (count options))
;       (move state (first options))
;       (move state (chose-option state options)))))

; (defn shortest-path-step
;   [{:keys [entrance] :as state}]
;   (loop [newstate state]
;     (println (:collected-keys newstate))
;     ; (println (select-keys newstate [:graph :steps :pos :keys :doors :collected-keys]))
;     (if (zero? (count (:keys newstate)))
;       (:steps newstate)
;       (recur (next-move newstate)))))

; (defn shortest-path-orig
;   [{:keys [entrance] :as state}]
;   (shortest-path-step (assoc (simplify-graph state)
;                              :collected-keys []
;                              :pos entrance
;                              :steps 0
;                              :laststep 0)))

; (defn node-name
;   [node]
;   (if (= :entrance node)
;     "@"
;     (second node)))


(defn all-routes-for-node
  [graph vertices node]
  (let [others (filter #(not= node %) vertices)]
    (zipmap others (map (partial g/shortest-distance graph node) others))))

; 

(defn fully-connected-keys-single
  [{:keys [keys entrances] :as graph}]
  (let [vertices (concat keys entrances)]
    (zipmap vertices (map (partial all-routes-for-node graph vertices) vertices))))

(defn path-needs
  [graph node1 node2]
  (let [path (g/dijkstra graph node1 node2)]
    (set (map str/lower-case (filter (partial door? graph) (butlast path))))))

(defn subgraph-needs
  [{:keys [keys entrances] :as graph}]
  (let [needs (zipmap keys
                      (map (partial path-needs graph (first entrances)) keys))]
    needs))

(defn subgraph
  [{:keys [keys doors] :as graph} entrance]
  (let [reachable (set (g/reachable graph entrance (constantly false)))]
    (assoc graph
           :graph (select-keys (:graph graph) (concat [entrance] reachable))
           :keys  (filter reachable keys)
           :doors (filter reachable doors)
           :entrances (list entrance))))

(defrecord LockedGraph [graph entrances needs]
  Graph
  (vertices
    [_]
    (keys graph))

  (edges
    [this v]
    (let [[collected-keys vertex] v
          all-available (conj collected-keys vertex)
          reachable (set (mapcat second (filter #(set/superset? all-available (first %)) needs)))
          not-yet-visited (set/difference (set (vertices this)) all-available)]
      (map (partial vector all-available) (set/intersection not-yet-visited reachable))))

  (distance
    [_ [a v1] [b v2]]
    ; (println v1 v2 (get-in graph [v1 v2]))
    (get-in graph [v1 v2])))

(defn fully-connected-keys
  [{:keys [entrances] :as graph}]
  (let [subgraphs (map (partial subgraph graph) entrances)
        newgraph (apply merge (map fully-connected-keys-single subgraphs))
        needs (apply merge (map subgraph-needs subgraphs))]
    (map->LockedGraph
     (assoc graph
            :graph newgraph
            :needs (group-by needs (keys needs))))))

(defn locked-dijkstra
  [graph]
  (let [max-keys (inc (count (vertices graph)))
        entrances (:entrances graph)
        start [(set entrances) (first entrances)]
        init-state {:dist (priority-map start 0) :prev {}}]
    (loop [visited #{}
           vertex start
           state init-state]
      (println vertex state)
      (if (= max-keys (count visited))
        (reverse (g/dijkstra-retrace (:prev state) vertex))
        (let [neighbors (filter (complement visited) (edges graph vertex))
              new-state (reduce (partial g/dijkstra-update graph vertex) state neighbors)]
          (recur (conj visited vertex)
                 (ffirst (g/entries-not-in-set visited (state :dist)))
                 new-state))))))

(defn shortest-path
  [input]
  (let [graph (->> input
                   ;(g/pruned)
                  ;  simplify-graph
                   fully-connected-keys
                  ;  ->LockedGraph
                   )
        path (locked-dijkstra graph)]
    (println path)
    (g/path-distance graph path)))

(defn day18-part1-soln
  []
  (shortest-path (load-graph day18-input)))

(defn shortest-robot-path
  [input]
  (map #(g/reachable input % (partial g/leaf? input)) (:entrances input)))
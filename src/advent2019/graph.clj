(ns advent2019.graph
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defprotocol Graph
  (vertices [this] "A collection of all the vertices in the graph")
  (edges [this v] "A collection of the edges for the given vertex in the graph")
  (distance [this v1 v2] "The distance (or edge weight) between two vertices"))

(defrecord MapGraph [graph]
  Graph
  (vertices
    [_]
    (keys graph))

  (edges
    [_ v]
    (keys (graph v)))

  (distance
    [_ v1 v2]
    (get-in graph [v1 v2])))

(defn degree
  "The degree of a vertex is the number of edges it has"
  [g v]
  (count (edges g v)))

(defn leaf?
  "Whether a vertex is a leaf vertex (meaning that it has at most one edge)"
  [g v]
  (= 1 (degree g v)))

(defn junction?
  "Whether a vertex is a junction (meaning that it has more than two edges)"
  [g v]
  (> 2 (degree g v)))

(defn entries-in-set
  [s m]
  (filter (fn [[k _]] (s k)) m))

(defn entries-not-in-set
  [s m]
  (filter (fn [[k _]] ((complement s) k)) m))

(defn single-path
  "Return the only possible path traversal from the start vertex (presumed to be a leaf vertex)
   until reaching another leaf vertex or a vertex with more than one un-traversed edge"
  ([g v & {:keys [exclude]}]
   (loop [visited (if exclude [exclude v] [v])
          neighbors (if exclude (filter (complement #{exclude}) (edges g v)) (edges g v))]
     (if (or (> (count neighbors) 1) (= (count neighbors) 0))
       visited
       (recur (conj visited (first neighbors))
              (filter (complement (set visited)) (edges g (first neighbors))))))))

(defn all-paths
  "Find all the paths from a vertex reaching a leaf vertex or a vertex with more than one
  untraversed edges"
  [g v]
  (let [neighbors (edges g v)]
    (map #(single-path g % :exclude v) neighbors)))

(defn- dijkstra-update
  [graph vertex {:keys [dist prev] :as state} neighbor]
  (let [alt (+ (dist vertex) (distance graph vertex neighbor))]
    (if (or (nil? (dist neighbor)) (< alt (dist neighbor)))
      {:dist (assoc dist neighbor alt) :prev (assoc prev neighbor vertex)}
      state)))

(defn- dijkstra-retrace
  [prev-steps finish]
  (loop [vertex finish chain []]
    (if (nil? vertex)
      chain
      (recur (prev-steps vertex) (conj chain vertex)))))

(defn dijkstra
  "Executes Dijkstra's algorithm to identify the shortest path between the start and finish vertices"
  [graph start finish & {:keys [limit]}]
  (let [max-search (or limit (count (vertices graph)))
        init-state {:dist (priority-map start 0) :prev {}}]
    (loop [visited #{} vertex start state init-state]
      (if (or (= max-search (count visited)) (= vertex finish))
        (reverse (dijkstra-retrace (state :prev) finish))
        (let [neighbors (filter (complement visited) (edges graph vertex))
              new-state (reduce (partial dijkstra-update graph vertex) state neighbors)]
          (recur (conj visited vertex) (ffirst (entries-not-in-set visited (state :dist))) new-state))))))
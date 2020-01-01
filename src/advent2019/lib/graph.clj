(ns advent2019.lib.graph
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.math.combinatorics :as combo]
            [advent2019.lib.utils :as u]))

(defprotocol Graph
  (vertices [this] "A collection of all the vertices in the graph")
  (vertex [this v] "Any data/information/label associated with the given vertex in the graph")
  (edges [this v] "A collection of the edges for the given vertex in the graph")
  (distance [this v1 v2] "The distance (or edge weight) between two vertices")
  (without-vertex [this v] "Produces a new graph with the vertex removed")
  (rewired-without-vertex [this v] "Produces a new graph, re-wired to preserve the transitive edges through the removed vertex"))

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
    (get-in graph [v1 v2]))

  (without-vertex
    [g v]
    (let [neighbors (edges g v)
          newgraph (-> (reduce #(update %1 %2 dissoc v) graph neighbors)
                       (dissoc v))]
      (assoc g :graph newgraph)))

  (rewired-without-vertex
    [g v]
    (let [neighbors (edges g v)
          all-pairs (combo/permuted-combinations neighbors 2)
          newedge-fn (fn [g [v1 v2]]
                       (update-in g [:graph v1] assoc v2 (+ (distance g v1 v)
                                                            (distance g v v2))))]
      (without-vertex (reduce newedge-fn g all-pairs) v))))

(defrecord LabeledMapGraph [graph]
  Graph
  (vertices
    [_]
    (keys graph))

  (vertex
    [_ v]
    (graph v))

  (edges
    [_ v]
    (keys (:edges (graph v))))

  (distance
    [_ v1 v2]
    (get-in graph [:edges v1 v2]))

  (without-vertex
    [_ v]
    (let [neighbors (keys (:edges graph v))
          newgraph (-> (reduce #(update-in %1 [:edges %2] dissoc v) graph neighbors)
                       (dissoc v))]
      (->LabeledMapGraph newgraph))))

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
  (> (degree g v) 2))

(defn path-distance
  "Computes the distance along a path (an ordered collection of vertices)"
  [g path]
  (reduce + (map #(apply (partial distance g) %) (partition 2 1 path))))

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

(defn single-path-2
  "Return the only possible path traversal from the start vertex (presumed to be a leaf vertex)
   until reaching another leaf vertex or a vertex with more than one un-traversed edge"
  ([g v s stop-at]
   (loop [visited [s v]
          neighbors (filter (complement #{s}) (edges g v))]
     (if (or (> (count neighbors) 1)
             (= (count neighbors) 0)
             (some? (stop-at (last visited))))
       visited
       (recur (conj visited (first neighbors))
              (filter (complement (set visited)) (edges g (first neighbors))))))))

(defn all-paths
  "Find all the paths from a vertex reaching a leaf vertex or a vertex with more than one
  untraversed edges"
  [g v & {:keys [excludes]}]
  (let [neighbors (edges g v)
        stop-at (if excludes (set excludes) #{})]
    (map #(single-path-2 g % v stop-at) neighbors)))

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

(defn pruned
  "Prunes the single branches from a graph, excluding any vertices in the exclude-set"
  [graph exclude-set]
  (loop [newgraph graph]
    (let [dead-end-pred (every-pred (partial leaf? newgraph) (complement exclude-set))
          dead-ends (filter dead-end-pred (vertices newgraph))]
      (if (= 0 (count dead-ends))
        newgraph
        (recur (reduce without-vertex newgraph dead-ends))))))

(defn summarize-path
  [g path]
  [(first path) {(last path) (path-distance g path)}])

(defn adjacencies
  [graph]
  (let [leaves (filter (partial leaf? graph) (vertices graph))
        junctions (filter (partial junction? graph) (vertices graph))
        nodes (concat leaves junctions)]
    (->> (mapcat (partial all-paths graph) nodes)
         (map (partial summarize-path graph))
         (group-by first)
         (u/fmap #(apply merge (map second %))))))

(defn reachable
  [graph start stop-cond]
  (loop [visited #{start} ends #{} explore (edges graph start)]
    (let [filter-pred (every-pred (complement stop-cond) (complement visited))
          next-neighbors (filter filter-pred explore)]
      (if (zero? (count next-neighbors))
        ends
        (let [node (first next-neighbors)]
          (recur (conj visited node)
                 (if (leaf? graph node) (conj ends node) ends)
                 (concat explore (edges graph node))))))))
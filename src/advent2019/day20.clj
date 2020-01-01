(ns advent2019.day20
  (:require [clojure.string :as str]
            [advent2019.lib.ascii :as ascii]
            [advent2019.lib.graph :as g :refer [Graph ->MapGraph without-vertex edges distance]]
            [advent2019.lib.maze :as maze :refer [->Maze]]
            [advent2019.lib.utils :as u]))

(def day20-input (vec (u/puzzle-input "day20-input.txt")))

(defn maze-dims
  [maze]
  (let [height (- (count maze) 4)
        width (- (count (first maze)) 4)
        hole-width (->> (drop 2 maze)
                        butlast
                        butlast
                        (mapcat #(re-seq #"\s+" %))
                        (map count)
                        (apply max))
        maze-thickness (- width hole-width)
        hole-height (- height maze-thickness)]
    {:width width
     :height height
     :thickness (/ maze-thickness 2)
     :hole-width hole-width
     :hole-height hole-height}))

(defn only-labels
  [pairs]
  (filter #(some? (re-find #"\w{2}" (second %))) pairs))

(defn horizontal-labels
  [maze width offset]
  (->> (map str
            (subs (maze (+ 0 offset)) 2 (- width 2))
            (subs (maze (+ 1 offset)) 2 (- width 2)))
       (map-indexed vector)
       only-labels))

(defn vertical-labels
  [maze offset]
  (->> (map #(subs % offset (+ 2 offset)) (butlast (butlast (drop 2 maze))))
       (map-indexed vector)
       only-labels))

(defn fix-coord
  [{:keys [width height thickness]} inout side [pos label]]
  (let [val [label inout]]
    (case inout
      :outer (case side
               :top [[pos 0] val]
               :bot [[pos (dec height)] val]
               :lft [[0 pos] val]
               :rgt [[(dec width) pos] val])
      :inner (case side
               :top [[pos (dec thickness)] val]
               :bot [[pos (- height thickness)] val]
               :lft [[(dec thickness) pos] val]
               :rgt [[(- width thickness) pos] val]))))

(defn label-locations
  [maze]
  (let [{:keys [width height thickness hole-width hole-height] :as dims} (maze-dims maze)
        outer-top (map (partial fix-coord dims :outer :top) (horizontal-labels maze width 0))
        outer-bot (map (partial fix-coord dims :outer :bot) (horizontal-labels maze width (+ height 2)))
        outer-lft (map (partial fix-coord dims :outer :lft) (vertical-labels maze 0))
        outer-rgt (map (partial fix-coord dims :outer :rgt) (vertical-labels maze (+ width 2)))
        inner-top (map (partial fix-coord dims :inner :top) (horizontal-labels maze width (+ 2 thickness)))
        inner-bot (map (partial fix-coord dims :inner :bot) (horizontal-labels maze width (+ thickness hole-height)))
        inner-lft (map (partial fix-coord dims :inner :lft) (vertical-labels maze (+ 2 thickness)))
        inner-rgt (map (partial fix-coord dims :inner :rgt) (vertical-labels maze (+ thickness hole-width)))]
    (apply hash-map (apply concat (concat outer-top outer-bot outer-lft outer-rgt inner-top inner-bot inner-lft inner-rgt)))))

(defn trim-maze
  [maze]
  (let [width (count (first maze))
        trim (fn [s] (subs s 2 (- width 2)))
        clean (fn [s] (str/replace s #"\w" " "))]
    (map (comp clean trim) (butlast (butlast (drop 2 maze))))))

(def maze-map
  {\. :open
   \# :wall
   \  :nothing})

(defn labels->portals
  [labels]
  (let [by-label (group-by (comp first second) labels)]
    (u/fmap #(into {} (map (fn [[k v]] [(second v) k]) %)) by-label)))

(defn switch-side
  [side]
  (case side
    :inner :outer
    :outer :inner))

(defn portal-edges
  [portals]
  (->> (vals portals)
       (map vals)
       (mapcat (fn [[a b]] {a {b 1} b {a 1}}))))

(defn load-maze
  [maze]
  (let [labels  (label-locations maze)
        portals (labels->portals labels)
        start   (get-in portals ["AA" :outer])
        end     (get-in portals ["ZZ" :outer])
        themaze (->Maze (->> maze
                             trim-maze
                             (ascii/ascii->map maze-map)
                             (filter #(not= :nothing (val %)))
                             (into {}))
                        (partial = :open))
        graph   (-> themaze
                    maze/Maze->Graph
                    (g/pruned (set (keys labels))))]
    {:labels  (dissoc labels start end)
     :portals (dissoc portals "AA" "ZZ")
     :ends    {"AA" start "ZZ" end}
     :dims    (maze-dims maze)
     :graph   graph}))

(defn maze-with-portals
  [{:keys [graph portals] :as state}]
  (let [portal-edges (portal-edges portals)
        graph-with-portals (reduce
                            (fn [g [k v]]
                              (update-in g [:graph k] merge v))
                            graph portal-edges)]
    (assoc state :graph graph-with-portals)))

(defn solve-maze
  [maze]
  (let [state (maze-with-portals (load-maze maze))
        start (get-in state [:ends "AA"])
        end (get-in state [:ends "ZZ"])
        graph (:graph state)]
    (g/path-distance graph (g/dijkstra graph start end))))

(defn day20-part1-soln
  []
  (solve-maze day20-input))

(defrecord RecursiveMaze [lookup3d]
  Graph
  (vertices
    [_]
    (println "This would return an infinite collection"))

  (edges
    [_ v]
    (keys (lookup3d v)))

  (distance
    [_ v1 v2]
    ((lookup3d v1) v2)))

(defn top-layer
  [{:keys [ends portals graph]}]
  (-> (reduce without-vertex graph (map :outer (vals portals)))
      (g/pruned (conj (set (map :inner (vals portals))) (ends "AA") (ends "ZZ")))
      g/adjacencies
      ->MapGraph))

(defn lower-layer
  [{:keys [ends labels graph]}]
  (-> (reduce without-vertex graph [(ends "AA") (ends "ZZ")])
      (g/pruned (set (keys labels)))
      g/adjacencies
      ->MapGraph))

(defn append-if-portal
  [{:keys [labels portals]} [x y z] edges]
  (let [maybe-portal (labels [x y])]
    (if (nil? maybe-portal)
      edges
      (let [[name side] maybe-portal
            [newx newy] (get-in portals [name (switch-side side)])
            newcoord [newx newy (if (= side :inner) (inc z) (dec z))]]
        (assoc edges newcoord 1)))))

(defn to-3d
  [z m]
  (u/kmap #(conj % z) m))

(defn recursive-maze
  [state]
  (let [top-graph (top-layer state)
        lower-graph (lower-layer state)
        lookup-fn (fn [[x y z]]
                    (if (= z 0)
                      (append-if-portal state [x y z] (to-3d 0 (get-in top-graph [:graph [x y]])))
                      (append-if-portal state [x y z] (to-3d z (get-in lower-graph [:graph [x y]])))))]
    (->RecursiveMaze lookup-fn)))

(defn solve-recursive-maze
  [maze]
  (let [state (load-maze maze)
        start (conj (get-in state [:ends "AA"]) 0)
        end (conj (get-in state [:ends "ZZ"]) 0)
        rmaze (recursive-maze state)]
    (g/path-distance rmaze (g/dijkstra rmaze start end :limit 100000))))

(defn day20-part2-soln
  []
  (solve-recursive-maze day20-input))
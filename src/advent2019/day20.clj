(ns advent2019.day20
  (:require [clojure.string :as str]
            [advent2019.ascii :as ascii]
            [advent2019.graph :as g :refer [Graph distance]]
            [advent2019.maze :as maze :refer [->Maze]]
            [advent2019.utils :as u]))

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

(defn boundary?
  [{{:keys [width height thickness]} :dims} [x y]]
  (or (= 0 x)
      (= (dec width) x)
      (= 0 y)
      (= (dec height) y)
      (and (= (dec thickness) x) (> y (dec thickness)) (< y (- height thickness)))
      (and (= (- width thickness) x) (> y (dec thickness)) (< y (- height thickness)))
      (and (= (dec thickness) y) (> x (dec thickness)) (< x (- width thickness)))
      (and (= (- height thickness) y) (> x (dec thickness)) (< x (- width thickness)))))

(defn outside?
  [{{:keys [width height thickness]} :dims} [x y]]
  (or (= x -1)
      (= x width)
      (= y -1)
      (= y height)
      (and (= thickness x) (> y (dec thickness)) (< y (- height thickness)))
      (and (= (dec (- width thickness)) x) (> y (dec thickness)) (< y (- height thickness)))
      (and (= thickness y) (> x (dec thickness)) (< x (- width thickness)))
      (and (= (dec (- height thickness)) y) (> x (dec thickness)) (< x (- width thickness)))))

(defn switch-side
  [side]
  (case side
    :inner :outer
    :outer :inner))

(defn portal-replace
  [{:keys [labels portals]} pos]
  (let [[name side] (labels pos)]
    (when side
      (get-in portals [name (switch-side side)]))))

(defn neighbor-fixer
  [state pos n]
  (if (outside? state n)
    (portal-replace state pos)
    n))

(defn neighbor-coords
  [state pos]
  (let [neighbors (maze/adj-coords pos)]
    (if (boundary? state pos)
      (filter some? (mapv (partial neighbor-fixer state pos) neighbors))
      neighbors)))

(defn neighbors
  [state pos]
  (let [coords (neighbor-coords state pos)
        vals (map (:maze state) coords)]
    (zipmap coords vals)))

(defn all-open
  [maze]
  (map first (filter #(= :open (val %)) maze)))

(defn open-neighbors
  [state pos]
  (all-open (neighbors state pos)))

(defrecord PortalMaze [labels portals ends dims maze]
  Graph
  (vertices
    [_]
    (all-open maze))

  (edges
    [this v]
    (all-open (neighbors this v)))

  (distance
    [_ _ _]
    1))

(defn load-maze
  [maze]
  (let [labels  (label-locations maze)
        portals (labels->portals labels)
        start   (get-in portals ["AA" :outer])
        end     (get-in portals ["ZZ" :outer])]
    (map->PortalMaze
     {:labels  (dissoc labels start end)
      :portals (dissoc portals "AA" "ZZ")
      :ends    {"AA" start
                "ZZ" end}
      :dims    (maze-dims maze)
      :maze    (into {} (filter #(not= :nothing (val %))
                                (ascii/ascii->map maze-map (trim-maze maze))))})))

(defn simpler-maze
  [state]
  (let [start (get-in state [:ends "AA"])
        end (get-in state [:ends "ZZ"])]
    (maze/relabel-dead-paths state #{start end} :wall)))

(defn simpler-maze-portals
  [{:keys [ends portals] :as state}]
  (let [excludes (into #{(ends "AA") (ends "ZZ")} (map :inner (vals portals)))]
    (maze/relabel-dead-paths state excludes :wall)))

(defn find-shortest-path
  [state start finish]
  (g/dijkstra state start finish))

(defn solve-maze
  [maze]
  (let [state (load-maze maze)
        simplified (simpler-maze state)
        start (get-in state [:ends "AA"])
        end (get-in state [:ends "ZZ"])]
    (find-shortest-path simplified start end)))

(defn day20-part1-soln
  []
  (dec (count (solve-maze day20-input))))

(defn top-layer
  [{:keys [portals maze] :as state}]
  (let [outer-locs (map :outer (vals portals))]
    (assoc state :maze (merge maze (zipmap outer-locs (repeat :wall))))))

(defn lower-layer
  [{:keys [ends maze] :as state}]
  (let [wall-locs [(ends "AA") (ends "ZZ")]]
    (assoc state :maze (merge maze (zipmap wall-locs (repeat :wall))))))

(defn portal-replace-3d
  [{:keys [labels portals]} [x y z]]
  (let [[name side] (labels [x y])
        [newx newy] (get-in portals [name (switch-side side)])]
    [newx newy (if (= side :inner) (inc z) (dec z))]))

(defn neighbor-fixer-3d
  [state pos [x y z]]
  (if (outside? state [x y])
    (portal-replace-3d state pos)
    [x y z]))

(defn neighbor-coords-3d
  [state [x y z]]
  (let [neighbors (map conj (maze/adj-coords [x y]) (repeat z))]
    (if (boundary? state [x y])
      (filter some? (mapv (partial neighbor-fixer-3d state [x y z]) neighbors))
      neighbors)))

(defn neighbors-3d
  [state pos]
  (let [coords (neighbor-coords-3d state pos)
        vals (map (state :maze) coords)]
    (zipmap coords vals)))

(defn open-neighbors-3d
  [state pos]
  (all-open (neighbors-3d state pos)))

(defn recursive-maze
  [state]
  (let [top-maze-template ((simpler-maze-portals (top-layer state)) :maze)
        lower-maze-template ((simpler-maze (lower-layer state)) :maze)]
    (assoc state :maze (fn [[x y z]]
                         (if (= z 0)
                           (top-maze-template [x y])
                           (lower-maze-template [x y]))))))

; (defn find-shortest-path-3d
;   [state start finish]
;   (maze/dijkstra state 1000000000 open-neighbors-3d distance start finish))

; (defn solve-recursive-maze
;   [maze]
;   (let [state (load-maze maze)
;         start (conj (get-in state [:ends "AA"]) 0)
;         end (conj (get-in state [:ends "ZZ"]) 0)
;         rmaze (recursive-maze state)]
;     (find-shortest-path-3d rmaze start end)))

(defn open-neighbors2
  [maze pos]
  (all-open (maze/neighbors maze pos)))

(defn summarize-path
  [path]
  (map (fn [x] [(first x) {(last x) (dec (count x))}]) path))

(defn simple-border-adjacents
  [state]
  (let [maze (:maze state)]
    (->>  (filter (partial boundary? state) (all-open maze))
          (map #(g/single-path (->Maze maze (partial = :open)) %))
          (filter #(> (count %) 1))
          summarize-path
          (apply concat)
          (apply hash-map))))

(defn simple-intersection-adjacents
  [state]
  (let [maze (:maze state)]
    (->> (filter #(= 3 (count (open-neighbors2 maze %))) (all-open maze))
         (mapcat #(g/all-paths (->Maze maze (partial = :open)) %))
         summarize-path
         (group-by first)
         (u/fmap #(apply merge (map second %))))))

(defn to-graph
  [state]
  (merge (simple-border-adjacents state) (simple-intersection-adjacents state)))

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

(defrecord RecursiveMaze [labels portals ends dims maze]
  Graph
  (vertices
    [_]
    (println "This would return an infinite collection"))

  (edges
    [_ v]
    (keys (maze v)))

  (distance
    [_ v1 v2]
    ((maze v1) v2)))

(defn recursive-maze2
  [state]
  (let [top-graph (-> state top-layer simpler-maze-portals to-graph)
        lower-graph (-> state lower-layer simpler-maze to-graph)]
    (map->RecursiveMaze
     (assoc state :maze (fn [[x y z]]
                          (if (= z 0)
                            (append-if-portal state [x y z] (to-3d 0 (top-graph [x y])))
                            (append-if-portal state [x y z] (to-3d z (lower-graph [x y])))))))))

(defn find-shortest-path-3d
  [state start finish]
  (let [path (g/dijkstra state start finish :limit 100000)
        steps (map #(apply (partial distance state) %) (partition 2 1 path))]
    (println path steps)
    (reduce + steps)))

(defn solve-recursive-maze
  [maze]
  (let [state (load-maze maze)
        start (conj (get-in state [:ends "AA"]) 0)
        end (conj (get-in state [:ends "ZZ"]) 0)
        rmaze (recursive-maze2 state)]
    (find-shortest-path-3d rmaze start end)))
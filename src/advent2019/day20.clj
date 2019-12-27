(ns advent2019.day20
  (:require [clojure.string :as str]
            [advent2019.ascii :as ascii]
            [advent2019.maze :as maze]
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

(defn load-maze
  [maze]
  (let [labels (label-locations maze)
        portals (labels->portals labels)]
    {:labels labels
     :ends {"AA" (get-in portals ["AA" :outer])
            "ZZ" (get-in portals ["ZZ" :outer])}
     :portals (dissoc portals "AA" "ZZ")
     :dims (maze-dims maze)
     :maze (into {} (filter #(not= :nothing (val %)) (ascii/ascii->map maze-map (trim-maze maze))))}))

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
    (get-in portals [name (switch-side side)])))

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
        vals (map (state :maze) coords)]
    (zipmap coords vals)))

(defn all-open
  [maze]
  (map first (filter #(= :open (val %)) maze)))

(defn open-neighbors
  [state pos]
  (all-open (neighbors state pos)))

(defn distance
  [_ _]
  ;; Could compute the manhattan distance, but it's always going to be one for the maze
  ;; (u/manhattan p1 p2)
  1)

(defn find-shortest-path
  [state start finish]
  (let [max-search (count (all-open (state :maze)))]
    (maze/dijkstra state max-search open-neighbors distance start finish)))

(defn solve-maze
  [maze]
  (let [state (load-maze maze)
        start (get-in state [:ends "AA"])
        end (get-in state [:ends "ZZ"])]
    (find-shortest-path state start end)))

(defn day20-part1-soln
  []
  (count (solve-maze day20-input)))

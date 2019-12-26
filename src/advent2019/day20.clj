(ns advent2019.day20
  (:require [advent2019.utils :as u]))

(def day20-input (u/puzzle-input-vec "day20-input.txt"))

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
  (case inout
    :outer (case side
             :top [[pos 0] label]
             :bot [[pos (dec height)] label]
             :lft [[0 pos] label]
             :rgt [[(dec width)] label])
    :inner (case side
             :top [[pos (dec thickness)] label]
             :bot [[pos (- height thickness)] label]
             :lft [[(dec thickness) pos] label]
             :rgt [[(- width thickness) pos] label])))

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
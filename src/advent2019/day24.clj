(ns advent2019.day24
  (:require [advent2019.ascii :as ascii]
            [advent2019.maze :as maze]
            [advent2019.lib.utils :as u]))

(def bug-map
  {\. :space
   \# :bug
   \? :down})

(def day24-input (ascii/ascii->map bug-map (u/puzzle-input "day24-input.txt")))

(defn conway-rule
  [space pos neighbor-fn]
  (let [neighbors (neighbor-fn space pos)
        state (space pos)
        adjacent-bug-count (u/count-if neighbors #(= :bug (val %)))]
    (case state
      :space (if (or (= 1 adjacent-bug-count) (= 2 adjacent-bug-count))
               :bug
               :space)
      :bug   (if (= 1 adjacent-bug-count)
               :bug
               :space)
      state)))

(defn conway-step
  [space rule]
  (zipmap (keys space)
          (map (partial rule space) (keys space))))

(defn conway-rule-2d
  [space pos]
  (conway-rule space pos maze/neighbors))

(defn conway-step-2d
  [space]
  (conway-step space conway-rule-2d))

(defn biodiversity
  [space]
  (read-string (apply str "2r" (map {:bug 1 :space 0} (reverse (for [y (range 5) x (range 5)] (space [x y])))))))

(defn find-recurrence
  [space]
  (loop [grid space last (biodiversity space) seen #{}]
    (if (some? (seen last))
      last
      (let [nextgrid (conway-step-2d grid)
            nextbio (biodiversity nextgrid)]
        (recur nextgrid nextbio (conj seen last))))))

(defn day24-part1-soln
  []
  (find-recurrence day24-input))

(defn base-neighbor-coords
  [[x y z]]
  [[x (dec y) z] [(dec x) y z] [x (inc y) z] [(inc x) y z]])

(defn outer-top
  [coords [_ y z]]
  (if (= 0 y)
    (conj coords [2 1 (inc z)])
    coords))
(defn outer-bottom
  [coords [_ y z]]
  (if (= 4 y)
    (conj coords [2 3 (inc z)])
    coords))
(defn outer-left
  [coords [x _ z]]
  (if (= 0 x)
    (conj coords [1 2 (inc z)])
    coords))
(defn outer-right
  [coords [x _ z]]
  (if (= 4 x)
    (conj coords [3 2 (inc z)])
    coords))
(defn inner-top
  [coords [x y z]]
  (if (= [2 1] [x y])
    (into coords (for [x (range 5)] [x 0 (dec z)]))
    coords))
(defn inner-bottom
  [coords [x y z]]
  (if (= [2 3] [x y])
    (into coords (for [x (range 5)] [x 4 (dec z)]))
    coords))
(defn inner-left
  [coords [x y z]]
  (if (= [1 2] [x y])
    (into coords (for [y (range 5)] [0 y (dec z)]))
    coords))
(defn inner-right
  [coords [x y z]]
  (if (= [3 2] [x y])
    (into coords (for [y (range 5)] [4 y (dec z)]))
    coords))

(def appenders
  [outer-top outer-bottom outer-left outer-right inner-top inner-bottom inner-left inner-right])

(defn neighbors3d-coords
  [pos]
  (let [base-coords (base-neighbor-coords pos)]
    (reduce #(%2 %1 pos) base-coords appenders)))

(defn neighbors3d
  [space pos]
  (let [coords (neighbors3d-coords pos)
        vals (map space coords)]
    (zipmap coords vals)))

(defn space3d-init
  [level0 depth]
  (merge
   (zipmap (for [z (range (- depth) (inc depth))
                 y (range 5)
                 x (range 5)]
             [x y z])
           (cycle (flatten [(repeat 12 :space) :down (repeat 12 :space)])))
   (zipmap (map (fn [[x y]] [x y 0]) (keys level0))
           (vals (assoc level0 [2 2] :down)))))

(defn space3d-level
  [space z]
  (let [coords (for [x (range 5) y (range 5)] [x y])]
    (zipmap coords (map (fn [[x y]] (space [x y z])) coords))))

(defn conway-rule-3d
  [space pos]
  (conway-rule space pos neighbors3d))

(defn conway-step3d
  [space]
  (conway-step space conway-rule-3d))

(defn simulate
  [level0 steps]
  (let [depth (quot steps 2)
        space3d (space3d-init level0 depth)]
    (first (drop steps (iterate conway-step3d space3d)))))

(defn bug-count
  [space3d]
  (u/count-if space3d #(= :bug (val %))))

(defn day24-part2-soln
  []
  (bug-count (simulate day24-input 200)))
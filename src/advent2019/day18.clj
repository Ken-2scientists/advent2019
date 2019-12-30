(ns advent2019.day18
  (:require [clojure.string :as str]
            [advent2019.lib.ascii :as ascii]
            [advent2019.lib.graph :as g :refer [Graph ->MapGraph without-vertex edges vertices distance]]
            [advent2019.lib.maze :as maze :refer [->Maze]]
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
    {:entrance entrance
     :keys keys
     :doors doors
     :nodes nodes
     :maze (->Maze themaze (partial not= :wall))}))

;; Consider merging this back into the maze namespace
(defn adjacencies
  [{:keys [maze nodes]}]
  (let [leaves    (filter (partial g/leaf? maze) (vertices maze))
        junctions (filter (partial g/junction? maze) (vertices maze))
        vs (concat leaves junctions nodes)]
    (->> (mapcat #(g/all-paths maze % :excludes nodes) vs)
         (map (partial maze/summarize-path maze))
         (group-by first)
         (u/fmap #(apply merge (map second %))))))

(defn to-graph
  [state]
  (-> state
      (assoc :graph (->MapGraph (adjacencies state)))
      (dissoc :maze)))

;; TODO --- add a splice (wrong name?) function to the Graph protocol to remove a junction from the graph, but updating the
;; neighbors so they are now directly connected with the correct distances.


;;; Sketch of an algorithm
; Start at entrance.
; what keys are reachable?
   ;;; "reachable" means that there's a path between current position and key in question without passing through a door
   ; if only one key is reachable, move to get that key -> remove the corresponding door from the problem.
   ; if more than one key is reachable, which one will minimize total steps over time?  That's the hard part.

   ; Somehow, make the optimimum choice to go to a key.
   ; recur now, with the newly available information
(defn find-path
  [{:keys [entrance keys doors graph]}]
  (let [start entrance]
    ;; Maybe compute all reachable points and then limit the reachable set to keys?
    (reachable-keys g pos)))
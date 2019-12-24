(ns advent2019.day24
  (:require [advent2019.ascii :as ascii]
            [advent2019.maze :as maze]
            [advent2019.utils :as u]))

(def bug-map
  {\. :space
   \# :bug})

(def day24-input (ascii/ascii->map bug-map (u/puzzle-input "day24-input.txt")))

(def d24-s1 (ascii/ascii->map bug-map ["....#"
                                       "#..#."
                                       "#..##"
                                       "..#.."
                                       "#...."]))

(defn conway-rule
  [space pos]
  (let [neighbors (maze/better-neighbors space pos)
        state (space pos)
        adjacent-bug-count (u/count-if neighbors #(= :bug (val %)))]
    (if (= :space state)
      (if (or (= 1 adjacent-bug-count) (= 2 adjacent-bug-count))
        :bug
        :space)
      (if (= 1 adjacent-bug-count)
        :bug
        :space))))

(defn conway-step
  [space]
  (zipmap (keys space)
          (map (partial conway-rule space) (keys space))))

(defn biodiversity
  [space]
  (read-string (apply str "2r" (map {:bug 1 :space 0} (reverse (for [y (range 5) x (range 5)] (space [x y])))))))

(defn find-recurrence
  [space]
  (loop [grid space last (biodiversity space) seen #{}]
    (if (some? (seen last))
      last
      (let [nextgrid (conway-step grid)
            nextbio (biodiversity nextgrid)]
        (recur nextgrid nextbio (conj seen last))))))

(defn day24-part1-soln
  []
  (find-recurrence day24-input))
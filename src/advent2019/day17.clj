(ns advent2019.day17
  (:require [clojure.string :as str]
            [advent2019.ascii :as ascii]
            [advent2019.intcode :as intcode]
            [advent2019.maze :as maze]
            [advent2019.utils :as u]))

(def day17-input (u/puzzle-input-vec "day17-input.txt"))

(def scaffold-mapping
  {\. :space
   \# :scaffold
   \^ :robot-up
   \> :robot-right
   \v :robot-down
   \< :robot-left
   \X :tumbling-robot})

(defn scaffold-map
  [ascii]
  (let [lines (str/split (str/join (map char ascii)) #"\n")]
    (ascii/ascii->map scaffold-mapping lines)))

(defn day17-map
  []
  (->> (intcode/intcode-ex day17-input [])
       intcode/read-output
       scaffold-map))

(defn intersection?
  [space pos]
  (if (= :scaffold (space pos))
    (every? #(= :scaffold %) (vals (maze/neighbors space pos)))
    false))

(defn intersections
  [space]
  (filter (partial intersection? space) (keys space)))

(defn alignment-sum
  [intersections]
  (reduce + (map #(apply * %) intersections)))

(defn day17-part1-soln
  []
  (->> (day17-map)
       intersections
       alignment-sum))

(def path
  (str
   "A,B,B,C,C,A,B,B,C,A\n"
   "R,4,R,12,R,10,L,12\n"
   "L,12,R,4,R,12\n"
   "L,12,L,8,R,10\n"
   "n\n"))

(defn day17-part2-soln
  []
  (let [code (assoc day17-input 0 2)]
    (->> path
         (map (comp int char))
         (intcode/intcode-ex code)
         intcode/read-output
         last)))

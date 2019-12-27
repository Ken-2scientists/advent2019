(ns advent2019.day20-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day20 :as t]))

(def d20-s1
  ["         A           "
   "         A           "
   "  #######.#########  "
   "  #######.........#  "
   "  #######.#######.#  "
   "  #######.#######.#  "
   "  #######.#######.#  "
   "  #####  B    ###.#  "
   "BC...##  C    ###.#  "
   "  ##.##       ###.#  "
   "  ##...DE  F  ###.#  "
   "  #####    G  ###.#  "
   "  #########.#####.#  "
   "DE..#######...###.#  "
   "  #.#########.###.#  "
   "FG..#########.....#  "
   "  ###########.#####  "
   "             Z       "
   "             Z       "])

(def d20-s2
  ["                   A               "
   "                   A               "
   "  #################.#############  "
   "  #.#...#...................#.#.#  "
   "  #.#.#.###.###.###.#########.#.#  "
   "  #.#.#.......#...#.....#.#.#...#  "
   "  #.#########.###.#####.#.#.###.#  "
   "  #.............#.#.....#.......#  "
   "  ###.###########.###.#####.#.#.#  "
   "  #.....#        A   C    #.#.#.#  "
   "  #######        S   P    #####.#  "
   "  #.#...#                 #......VT"
   "  #.#.#.#                 #.#####  "
   "  #...#.#               YN....#.#  "
   "  #.###.#                 #####.#  "
   "DI....#.#                 #.....#  "
   "  #####.#                 #.###.#  "
   "ZZ......#               QG....#..AS"
   "  ###.###                 #######  "
   "JO..#.#.#                 #.....#  "
   "  #.#.#.#                 ###.#.#  "
   "  #...#..DI             BU....#..LF"
   "  #####.#                 #.#####  "
   "YN......#               VT..#....QG"
   "  #.###.#                 #.###.#  "
   "  #.#...#                 #.....#  "
   "  ###.###    J L     J    #.#.###  "
   "  #.....#    O F     P    #.#...#  "
   "  #.###.#####.#.#####.#####.###.#  "
   "  #...#.#.#...#.....#.....#.#...#  "
   "  #.#####.###.###.#.#.#########.#  "
   "  #...#.#.....#...#.#.#.#.....#.#  "
   "  #.###.#####.###.###.#.#.#######  "
   "  #.#.........#...#.............#  "
   "  #########.###.###.#############  "
   "           B   J   C               "
   "           U   P   P               "])

(deftest label-locations-test
  (testing "Can find the maze coordinates of all of the labels"
    (is (= {[7 0] "AA", 
            [0 6] "BC", [0 11] "DE", [0 13] "FG", 
            [7 4] "BC", 
            [4 8] "DE", 
            [9 10] "FG", 
            [11 14] "ZZ"}
           (t/label-locations d20-s1)))
    (is (= {[17 0] "AA"
            [0 13] "DI", [0 17] "JO", [0 21] "YN",
            [9 32] "BU", [13 32] "JP", [17 32] "CP",
            [30 9] "VT", [30 15] "AS", [30 19] "LF", [30 21] "QG",
            [15 6] "AS", [19 6] "CP", 
            [6 19] "DI"
            [11 26] "JO", [13 26] "LF", [19 26] "JP",
            [24 11] "YN", [24 15] "QG", [24 19] "BU", [24 21] "VT",  
            [0 15] "ZZ"}
           (t/label-locations d20-s2)))))

(deftest boundary-test
  (testing "Can identify when the position is an inner or outer boundary"
    (let [state {:dims (t/maze-dims d20-s1)} ]
      (is (= true (t/boundary? state [0 0])))
      (is (= false (t/boundary? state [1 1])))
      (is (= true (t/boundary? state [16 3])))
      (is (= true (t/boundary? state [0 14])))
      (is (= true (t/boundary? state [4 14])))      
      (is (= true (t/boundary? state [5 4])))
      (is (= false (t/boundary? state [4 4])))
      (is (= true (t/boundary? state [4 5])))
      (is (= true (t/boundary? state [11 4])))
      (is (= false (t/boundary? state [12 4])))
      (is (= true (t/boundary? state [12 5])))
      (is (= true (t/boundary? state [4 9])))
      (is (= false (t/boundary? state [4 10])))
      (is (= true (t/boundary? state [5 10])))
      (is (= true (t/boundary? state [11 10])))
      (is (= false (t/boundary? state [12 10])))
      (is (= true (t/boundary? state [12 9]))))))

(deftest outside-test
  (testing "Can identify when the position is 'outside' the maze"
    (let [state {:dims (t/maze-dims d20-s1)}]
      (is (= true (t/outside? state [-1 0])))
      (is (= false (t/outside? state [0 0])))
      (is (= true (t/outside? state [17 3])))
      (is (= true (t/outside? state [-1 14])))
      (is (= true (t/outside? state [4 15])))
      (is (= true (t/outside? state [5 5])))
      (is (= true (t/outside? state [11 5])))
      (is (= true (t/outside? state [5 9])))
      (is (= true (t/outside? state [11 9]))))))

(deftest neighbor-coords-test
  (testing "Neighbor lookup follows portals"
    (let [state (t/load-maze d20-s1)]
      (is (= [[7 3] [6 4] [0 6] [8 4]] (t/neighbor-coords state [7 4])))
      (is (= [[4 7] [3 8] [4 9] [0 11]] (t/neighbor-coords state [4 8])))
      (is (= [[0 12] [9 10] [0 14] [1 13]] (t/neighbor-coords state [0 13]))))))

(deftest shortest-path-test
  (testing "Can find the shortest path when using portals"
    (is (= 23 (count (t/solve-maze d20-s1))))
    (is (= 58 (count (t/solve-maze d20-s2))))))

(deftest day20-part1-soln-test
  (testing "Can reproduce the solution for part1"
    (is (= 578 (t/day20-part1-soln)))))
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
    (is (= {[7 0] ["AA" :outer], 
            [0 6] ["BC" :outer], [0 11] ["DE" :outer], [0 13] ["FG" :outer], 
            [7 4] ["BC" :inner], 
            [4 8] ["DE" :inner], 
            [9 10] ["FG" :inner], 
            [11 14] ["ZZ" :outer]}
           (t/label-locations d20-s1)))
    (is (= {[17 0] ["AA" :outer]
            [0 13] ["DI" :outer], [0 17] ["JO" :outer], [0 21] ["YN" :outer],
            [9 32] ["BU" :outer], [13 32] ["JP" :outer], [17 32] ["CP" :outer],
            [30 9] ["VT" :outer], [30 15] ["AS" :outer], [30 19] ["LF" :outer], [30 21] ["QG" :outer],
            [15 6] ["AS" :inner], [19 6] ["CP" :inner], 
            [6 19] ["DI" :inner]
            [11 26] ["JO" :inner], [13 26] ["LF" :inner], [19 26] ["JP" :inner],
            [24 11] ["YN" :inner], [24 15] ["QG" :inner], [24 19] ["BU" :inner], [24 21] ["VT" :inner],  
            [0 15] ["ZZ" :outer]}
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
    (is (= 23 (dec (count (t/solve-maze d20-s1)))))
    (is (= 58 (dec (count (t/solve-maze d20-s2)))))))

(deftest day20-part1-soln-test
  (testing "Can reproduce the solution for part1"
    (is (= 578 (t/day20-part1-soln)))))

(def d20-s3 
  [
   "             Z L X W       C                 "
   "             Z P Q B       K                 "
   "  ###########.#.#.#.#######.###############  "
   "  #...#.......#.#.......#.#.......#.#.#...#  "
   "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  "
   "  #.#...#.#.#...#.#.#...#...#...#.#.......#  "
   "  #.###.#######.###.###.#.###.###.#.#######  "
   "  #...#.......#.#...#...#.............#...#  "
   "  #.#########.#######.#.#######.#######.###  "
   "  #...#.#    F       R I       Z    #.#.#.#  "
   "  #.###.#    D       E C       H    #.#.#.#  "
   "  #.#...#                           #...#.#  "
   "  #.###.#                           #.###.#  "
   "  #.#....OA                       WB..#.#..ZH"
   "  #.###.#                           #.#.#.#  "
   "CJ......#                           #.....#  "
   "  #######                           #######  "
   "  #.#....CK                         #......IC"
   "  #.###.#                           #.###.#  "
   "  #.....#                           #...#.#  "
   "  ###.###                           #.#.#.#  "
   "XF....#.#                         RF..#.#.#  "
   "  #####.#                           #######  "
   "  #......CJ                       NM..#...#  "
   "  ###.#.#                           #.###.#  "
   "RE....#.#                           #......RF"
   "  ###.###        X   X       L      #.#.#.#  "
   "  #.....#        F   Q       P      #.#.#.#  "
   "  ###.###########.###.#######.#########.###  "
   "  #.....#...#.....#.......#...#.....#.#...#  "
   "  #####.#.###.#######.#######.###.###.#.#.#  "
   "  #.......#.......#.#.#.#.#...#...#...#.#.#  "
   "  #####.###.#####.#.#.#.#.###.###.#.###.###  "
   "  #.......#.....#.#...#...............#...#  "
   "  #############.#.#.###.###################  "
   "               A O F   N                     "
   "               A A D   M                     "])

(deftest shortest-path-3d-test
  (testing "Can find the shortest path through the recursive maze"
    (is (= 26 (t/solve-recursive-maze d20-s1)))
    (is (= 396 (t/solve-recursive-maze d20-s3)))))

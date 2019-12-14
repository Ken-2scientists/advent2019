(ns advent2019.day10-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day10 :as t]))


(def d10-s1 (t/parse-map
             [".#..#"
              "....."
              "#####"
              "....#"
              "...##"]))

(def d10-s2 (t/parse-map
             ["......#.#."
              "#..#.#...."
              "..#######."
              ".#.#.###.."
              ".#..#....."
              "..#....#.#"
              "#..#....#."
              ".##.#..###"
              "##...#..#."
              ".#....####"]))

(def d10-s3 (t/parse-map
             ["#.#...#.#."
              ".###....#."
              ".#....#..."
              "##.#.#.#.#"
              "....#.#.#."
              ".##..###.#"
              "..#...##.."
              "..##....##"
              "......#..."
              ".####.###."]))

(def d10-s4 (t/parse-map
             [".#..#..###"
              "####.###.#"
              "....###.#."
              "..###.##.#"
              "##.##.#.#."
              "....###..#"
              "..#.#..#.#"
              "#..#.#.###"
              ".##...##.#"
              ".....#.#.."]))

(def d10-s5 (t/parse-map
             [".#..##.###...#######"
              "##.############..##."
              ".#.######.########.#"
              ".###.#######.####.#."
              "#####.##.#.##.###.##"
              "..#####..#.#########"
              "####################"
              "#.####....###.#.#.##"
              "##.#################"
              "#####.##.###..####.."
              "..######..##.#######"
              "####.##.####...##..#"
              ".#####..#.######.###"
              "##...#.##########..."
              "#.##########.#######"
              ".####.#.###.###.#.##"
              "....##.##.###..#####"
              ".#.#.###########.###"
              "#.#.#.#####.####.###"
              "###.##.####.##.#..##"]))

(def d10-s6 (t/parse-map
             [".#....#####...#.."
              "##...##.#####..##"
              "##...#...#.#####."
              "..#.....#...###.."
              "..#.#.....#....##"]))

(deftest best-location-test
  (testing "Can find best location and max visible count"
    (is (= '([3,4] 8) (t/best-location d10-s1)))
    (is (= '([5,8] 33) (t/best-location d10-s2)))
    (is (= '([1,2] 35) (t/best-location d10-s3)))
    (is (= '([6,3] 41) (t/best-location d10-s4)))
    (is (= '([11,13] 210) (t/best-location d10-s5)))))

(deftest day10-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 253 (second (t/day10-part1-soln))))))

(def d10-s6-soln
  [[8,1] [9,0] [9,1] [10,0] [9,2] [11,1] [12,1] [11,2] [15,1]
   [12,2] [13,2] [14,2] [15,2] [12,3] [16,4] [15,4] [10,4] [4,4]
   [2,4] [2,3] [0,2] [1,2] [0,1] [1,1] [5,2] [1,0] [5,1]
   [6,1] [6,0] [7,0] [8,0] [10,1] [14,0] [16,1] [13,3] [14,3]])


(deftest laser-test
  (testing "Laser hits the asteroids in the correct order"
    (is (= d10-s6-soln (t/asteroids-laser-order [8,3] d10-s6)))
    (let [larger-test (t/asteroids-laser-order [11,13] d10-s5)]
      (is (= [11,12] (nth larger-test 0)))
      (is (= [12,1] (nth larger-test 1)))
      (is (= [12,2] (nth larger-test 2)))
      (is (= [12,8] (nth larger-test 9)))
      (is (= [16,0] (nth larger-test 19)))
      (is (= [16,9] (nth larger-test 49)))
      (is (= [10,16] (nth larger-test 99)))
      (is (= [9,6] (nth larger-test 198)))
      (is (= [8,2] (nth larger-test 199)))
      (is (= [10,9] (nth larger-test 200)))
      (is (= [11,1] (nth larger-test 298))))))

(deftest day10-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 815 (t/day10-part2-soln)))))
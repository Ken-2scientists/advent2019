(ns advent2019.day24-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.ascii :as a]
            [advent2019.day24 :as t]))

(def d24-s1 (a/ascii->map t/bug-map ["....#" "#..#." "#..##" "..#.." "#...."]))
(def d24-s1-step1 (a/ascii->map t/bug-map ["#..#." "####." "###.#" "##.##" ".##.."]))
(def d24-s1-step2 (a/ascii->map t/bug-map ["#####" "....#" "....#" "...#." "#.###"]))
(def d24-s1-step3 (a/ascii->map t/bug-map ["#...." "####." "...##" "#.##." ".##.#"]))
(def d24-s1-step4 (a/ascii->map t/bug-map ["####." "....#" "##..#" "....." "##..."]))

(def d24-s2 (a/ascii->map t/bug-map ["....." "....." "....." "#...." ".#..."]))

(deftest conway-step-test
  (testing "Can properly execute the rules for Conway's game of life"
    (is (= d24-s1-step1 (t/conway-step d24-s1)))
    (is (= d24-s1-step2 (t/conway-step d24-s1-step1)))
    (is (= d24-s1-step3 (t/conway-step d24-s1-step2)))
    (is (= d24-s1-step4 (t/conway-step d24-s1-step3)))))

(deftest biodiversity-test
  (testing "Can properly compute biodiversity"
    (is (= 2129920 (t/biodiversity d24-s2)))))

(deftest day24-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 17863711 (t/day24-part1-soln)))))


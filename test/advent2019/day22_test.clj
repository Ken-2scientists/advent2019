(ns advent2019.day22-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day22 :as t]))

(def d22-s1
  [[:increment 7]
   [:deal]
   [:deal]])

(def d22-s2
  [[:cut 6]
   [:increment 7]
   [:deal]])

(def d22-s3
  [[:increment 7]
   [:increment 9]
   [:cut -2]])

(def d22-s4
  [[:deal]
   [:cut -2]
   [:increment 7]
   [:cut 8]
   [:cut -4]
   [:increment 7]
   [:cut 3]
   [:increment 9]
   [:increment 3]
   [:cut -1]])

(deftest shuffle-test
  (testing "Shuffle operations work on small decks"
    (is (= [0 3 6 9 2 5 8 1 4 7] (t/shuffle-deck 10 d22-s1)))
    (is (= [3 0 7 4 1 8 5 2 9 6] (t/shuffle-deck 10 d22-s2)))
    (is (= [6 3 0 7 4 1 8 5 2 9] (t/shuffle-deck 10 d22-s3)))
    (is (= [9 2 5 8 1 4 7 0 3 6] (t/shuffle-deck 10 d22-s4)))))

(deftest day22-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= 2306 (t/day22-part1-soln)))))

(deftest multiple-shuffle-test
  (testing "Correctly identify the card in given position after many repeated shuffles"
    (is (= 9 (t/card-after-multiple-shuffles 10 d22-s1 5 3)))
    (is (= 8 (t/card-after-multiple-shuffles 10 d22-s2 5 5)))
    (is (= 8 (t/card-after-multiple-shuffles 10 d22-s3 10 0)))))

(deftest day22-part2-soln-test
  (testing "Can reproduce the answer for part2"
    (is (= 12545532223512 (t/day22-part2-soln)))))

(ns advent2019.day16-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day16 :as t]))

(deftest phase-test
  (testing "Correctly apply phases"
    (is (= ["48226158" "34040438" "03415518" "01029498"]
           (take 4 (rest (iterate t/phase "12345678")))))
    (is (= "24176176"
           (subs (nth (iterate t/phase "80871224585914546619083218645595") 100) 0 8)))
    (is (= "73745418"
           (subs (nth (iterate t/phase "19617804207202209144916044189917") 100) 0 8)))
    (is (= "52432133"
           (subs (nth (iterate t/phase "69317163492948606335995924319873") 100) 0 8)))))

(deftest day16-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= "70856418" (t/day16-part1-soln)))))
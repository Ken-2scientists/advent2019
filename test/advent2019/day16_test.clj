(ns advent2019.day16-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.day16 :as t]))

(deftest phase-test
  (testing "Correctly apply phases"
    (is (= ["12345678" "48226158" "34040438" "03415518" "01029498"]
           (->> (t/str->nums "12345678")
                (iterate t/phase)
                (take 5)
                (map t/nums->str))))
    (is (= "24176176"
           (t/nums->str (take 8 (t/run-phases (t/str->nums "80871224585914546619083218645595") 100)))))
    (is (= "73745418"
           (t/nums->str (take 8 (t/run-phases (t/str->nums "19617804207202209144916044189917") 100)))))
    (is (= "52432133"
           (t/nums->str (take 8 (t/run-phases (t/str->nums "69317163492948606335995924319873") 100)))))))

(deftest day16-part1-soln-test
  (testing "Can reproduce the answer for part1"
    (is (= "70856418" (t/day16-part1-soln)))))
(ns advent2019.day05-test
  (:require [clojure.test :refer :all]
            [advent2019.day05 :refer :all]))

; (deftest can-parse-instruction
;   (testing "Can parse instructions correctly"
;     (is (= {:operation :read
;             :params 1
;             :size 2
;             :param-types '(:position)}
;            (parse-instruction 3)))
;     (is (= {:operation :multiply
;             :params 3
;             :size 4
;             :param-types '(:position :immediate :position)}
;            (parse-instruction 1002)))))

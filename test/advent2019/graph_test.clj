(ns advent2019.graph-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.graph :as g])
  (:import advent2019.graph.MapGraph))

(def t1 (MapGraph. {:a {:b 1}
                    :b {:a 1 :c 2}
                    :c {:b 2 :d 3}
                    :d {:c 3 :e 1}
                    :e {:d 1}}))

(def t2 (MapGraph. {:a {:b 1}
                    :b {:a 1 :c 2 :f 4}
                    :c {:b 2 :d 3}
                    :d {:c 3 :e 1}
                    :e {:d 1}
                    :f {:b 4 :g 1}
                    :g {:f 1}}))

(def t3 (MapGraph. {:a {:b 7 :c 14 :d 9}
                    :b {:a 7 :d 10 :e 15}
                    :c {:a 14 :d 2 :f 9}
                    :d {:a 9 :b 10 :c 2 :e 11}
                    :e {:b 15 :d 11 :f 6}
                    :f {:c 9 :e 6}}))

(deftest single-path-test
  (testing "Can traverse a graph until its end or a junction is reached"
    (is (= [:a :b :c :d :e] (g/single-path t1 :a)))
    (is (= [:a :b]          (g/single-path t2 :a)))
    (is (= [:g :f :b]       (g/single-path t2 :g)))))

(deftest all-paths-test
  (testing "Can traverse a graph until its end or a junction is reached"
    (is (= [[:b :a] [:b :c :d :e] [:b :f :g]]
           (g/all-paths t2 :b)))
    (is (= [[:g :f :b]]
           (g/all-paths t2 :g)))))

(deftest dijkstra-test
  (testing "Can find the shortest path between two vertices"
    (is (= [:a :d :c :f] (g/dijkstra t3 :a :f)))))
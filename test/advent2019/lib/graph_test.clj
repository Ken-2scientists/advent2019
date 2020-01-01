(ns advent2019.lib.graph-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.lib.graph :as g :refer [without-vertex rewired-without-vertex ->MapGraph]]))

(def t1 (->MapGraph {:a {:b 1}
                     :b {:a 1 :c 2}
                     :c {:b 2 :d 3}
                     :d {:c 3 :e 1}
                     :e {:d 1}}))

(def t2 (->MapGraph {:a {:b 1}
                     :b {:a 1 :c 2 :f 4}
                     :c {:b 2 :d 3}
                     :d {:c 3 :e 1}
                     :e {:d 1}
                     :f {:b 4 :g 1}
                     :g {:f 1}}))

(def t3 (->MapGraph {:a {:b 7 :c 14 :d 9}
                     :b {:a 7 :d 10 :e 15}
                     :c {:a 14 :d 2 :f 9}
                     :d {:a 9 :b 10 :c 2 :e 11}
                     :e {:b 15 :d 11 :f 6}
                     :f {:c 9 :e 6}}))

(def t4 (->MapGraph {:a {:j 3}
                     :b {:j 2}
                     :c {:j 4}
                     :d {:j 6}
                     :j {:a 3 :b 2 :c 4 :d 6}}))

(deftest without-vertex-test
  (testing "Can return a new graph with a vertex (and its corresponding edges) removed"
    (is (= (->MapGraph {:b {:c 2}, :c {:b 2, :d 3}, :d {:c 3, :e 1}, :e {:d 1}})
           (without-vertex t1 :a)))))

(deftest rewired-without-vertex-test
  (testing "Can return a new graph with a vertex removed, preserving the transitive relationships"
    (is (= (->MapGraph {:a {:b 5 :c 7 :d 9}
                        :b {:a 5 :c 6 :d 8}
                        :c {:a 7 :b 6 :d 10}
                        :d {:a 9 :b 8 :c 10}})
           (rewired-without-vertex t4 :j)))))

(deftest pruned-test
  (testing "Can prune a graph of unnecessary branches"
    (is (= (g/pruned t2 #{:a :g})
           (->MapGraph {:a {:b 1}
                        :b {:a 1 :f 4}
                        :f {:b 4 :g 1}
                        :g {:f 1}})))))

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
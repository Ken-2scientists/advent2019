(ns advent2019.maze-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent2019.maze :as t]))

(def simple-graph {:a {:b 1}
                   :b {:a 1 :c 2}
                   :c {:b 2 :d 3}
                   :d {:c 3 :e 1}
                   :e {:d 1}})

(def graph-with-branch {:a {:b 1}
                        :b {:a 1 :c 2 :f 4}
                        :c {:b 2 :d 3}
                        :d {:c 3 :e 1}
                        :e {:d 1}
                        :f {:b 4 :g 1}
                        :g {:f 1}})

(defn edges-fn
  [graph node]
  (keys (graph node)))

(deftest path-following-test
  (testing "Can traverse a graph until its end or a junction is reached"
    (is (= [:a :b :c :d :e] (t/only-available-path simple-graph edges-fn :a)))
    (is (= [:a :b]          (t/only-available-path graph-with-branch edges-fn :a)))
    (is (= [:g :f :b]       (t/only-available-path graph-with-branch edges-fn :g)))))

(deftest paths-from-junction-test
  (testing "Can traverse a graph until its end or a junction is reached"
    (is (= [[:b :a] [:b :c :d :e] [:b :f :g]]
           (t/paths-from-junction graph-with-branch edges-fn :b)))))
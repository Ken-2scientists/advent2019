(ns advent2019.utils
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn puzzle-input
  [filename]
  (->> filename
       io/resource
       io/reader
       line-seq))

(defn list-line
  [line]
  (read-string (str "[" line "]")))

(defn puzzle-input-vec
  [filename]
  (-> filename puzzle-input first list-line))

(defn fmap
  "Applies the function f to the values of the map m"
  [f m]
  (zipmap (keys m) (map f (vals m))))

(defn kmap
  "Applies the function f to the keys of the map m"
  [f m]
  (zipmap (map f (keys m)) (vals m)))

(defn pprint-image
  [image width]
  (let [legible-pixels (replace {0 " " 1 "*"} image)]
    (doseq [line (partition width legible-pixels)]
      (println (str/join line)))))

(defn index-of
  [x coll]
  (ffirst (filter #(= x (second %)) (map-indexed vector coll))))

(defn mod-inverse
  "Computes the multiplicative inverse of a, mod m"
  [a m]
  (loop [t 0 next-t 1 r m next-r a]
    (if (zero? next-r)
      (if (neg? t) (+ t m) t)
      (let [q (quot r next-r)]
        (recur next-t (- t (* q next-t)) next-r (- r (* q next-r)))))))

(defn mod-quot
  "Computes a / b mod m"
  [a b m]
  (mod (*' a (mod-inverse b m)) m))

(defn count-if
  [coll pred]
  (count (filter pred coll)))


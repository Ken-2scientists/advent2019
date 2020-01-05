(ns advent2019.lib.utils
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

(defn pprint-image
  [image width]
  (let [legible-pixels (replace {0 " " 1 "*"} image)]
    (doseq [line (partition width legible-pixels)]
      (println (str/join line)))))

(defn fmap
  "Applies the function f to the values of the map m"
  [f m]
  (zipmap (keys m) (map f (vals m))))

(defn kmap
  "Applies the function f to the keys of the map m"
  [f m]
  (zipmap (map f (keys m)) (vals m)))

(defn without-keys
  "Returns a map with only the entries in map whose key isn't in keyseq"
  [map keyseq]
  (select-keys map (filter (complement (set keyseq)) (keys map))))

(defn rotate
  "Rotate the collection by n"
  [n coll]
  (let [size (count coll)]
    (take size (drop (mod n size) (cycle coll)))))

(defn index-of
  [x coll]
  (ffirst (filter #(= x (second %)) (map-indexed vector coll))))

(defn count-if
  [coll pred]
  (count (filter pred coll)))

(defn invert-map
  [m]
  (zipmap (vals m) (keys m)))
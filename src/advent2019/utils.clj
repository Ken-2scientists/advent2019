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
  [coll x]
  (ffirst (filter #(= x (second %)) (map-indexed (fn [idx v] [idx v]) coll))))


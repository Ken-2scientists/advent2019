(ns advent2019.utils
  (:require [clojure.java.io :as io]))

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


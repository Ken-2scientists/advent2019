(ns advent2019.utils
  (:require [clojure.java.io :as io]))

(defn load-file
  [filename]
  (->> filename
       io/resource
       io/reader
       line-seq))

(defn list-line
  [line]
  (read-string (str "[" line "]")))


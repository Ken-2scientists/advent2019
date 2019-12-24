(ns advent2019.ascii
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

(defn ascii->map
  [codes lines]
  (let [row-count (count lines)
        symbols (map #(map codes %) lines)
        col-count (count (first symbols))]
    (zipmap (for [y (range row-count)
                  x (range col-count)]
              [x y])
            (flatten symbols))))

(defn map->string
  [codes themap rows cols]
  (let [chars (u/invert-map codes)
        rep (partition cols (for [y (range rows)
                                  x (range cols)]
                              (chars (themap [x y]))))]
    (str/join "\n" (mapv #(apply str %) rep))))
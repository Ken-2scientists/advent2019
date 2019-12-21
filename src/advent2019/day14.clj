(ns advent2019.day14
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

(defn parse-component
  [comp-str]
  (let [[qty chem] (str/split comp-str #"\s+")]
    [(keyword chem) (read-string qty)]))

(defn parse-line
  [line]
  (let [[lhs rhs] (str/split line #" \=\> ")
        lhs-split (str/split lhs #", ")
        components (mapcat parse-component lhs-split)
        [chem qty] (parse-component rhs)]
    {chem {:min-qty qty :comps components}}))

(defn reactions
  [input]
  (->> input
       (map parse-line)
       (apply merge)))

(def day14-input
  (reactions (u/puzzle-input "day14-input.txt")))

(defn multiply
  [factor ingredients]
  (mapcat (fn [[chem qty]] [chem (* factor qty)]) (partition 2 ingredients)))

(defn consume
  [inventory used [chemical qty]]
  (swap! used update chemical #(+ % qty))
  (swap! inventory update chemical #(- % qty)))

(defn order-if-needed
  [recipes inventory used [chemical qty]]
  ;(println "Need " qty " of " chemical ", have " (get @inventory chemical))
  ; (println @inventory)
  ; (println @used)
  (if (= :ORE chemical)
    (swap! inventory update chemical #(+ % qty))
    (let [{:keys [min-qty comps]} (get recipes chemical)
          available (get @inventory chemical)
          order-amount (int (Math/ceil (/ (- qty available) min-qty)))
          multiplier (int (Math/ceil (/ qty min-qty)))]
      (doseq [[x q] (partition 2 comps)]
        (when (> (* order-amount q) (get @inventory x))
          ;(println chemical " needs " (* order-amount q) " of " x)
          (order-if-needed recipes inventory used [x (* order-amount q)]))
        (consume inventory used [x (* order-amount q)])
        ;(println chemical " used " (* order-amount q) " of " x)
        )
      (swap! inventory update chemical #(+ % (* order-amount min-qty)))
      ;(println "Made " (* order-amount min-qty) " of " chemical ", now have " (get @inventory chemical))
      )))

(defn empty-state
  [recipes]
  (zipmap (conj (keys recipes) :ORE) (repeat 0)))

(defn ingredients-used
  [recipes chemical qty]
  (let [inventory (atom (empty-state recipes))
        used (atom (empty-state recipes))]
    (order-if-needed recipes inventory used [chemical qty])
    {:remaining @inventory
     :consumed @used}))

(defn ore-amount
  [recipes]
  (get-in (ingredients-used recipes :FUEL 1) [:consumed :ORE]))

(defn day14-part1-soln
  []
  (ore-amount day14-input))
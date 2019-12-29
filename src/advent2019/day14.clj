(ns advent2019.day14
  (:require [clojure.string :as str]
            [advent2019.lib.utils :as u]))

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
  (if (= :ORE chemical)
    (swap! inventory update chemical #(+ % qty))
    (let [{:keys [min-qty comps]} (get recipes chemical)
          available (get @inventory chemical)
          order-amount (long (Math/ceil (/ (- qty available) min-qty)))]
      (doseq [[x q] (partition 2 comps)]
        (when (> (* order-amount q) (get @inventory x))
          (order-if-needed recipes inventory used [x (* order-amount q)]))
        (consume inventory used [x (* order-amount q)]))
      (swap! inventory update chemical #(+ % (* order-amount min-qty))))))

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
  ([recipes]
   (ore-amount recipes 1))
  ([recipes amount]
   (get-in (ingredients-used recipes :FUEL amount) [:consumed :ORE])))

(defn day14-part1-soln
  []
  (ore-amount day14-input))

(def available-ore 1000000000000)

(defn binary-search
  ([f target start end]
   (let [fixed-end (if (even? (- end start))
                     (inc end)
                     end)]
     (loop [low start high fixed-end]
       (if (= high (inc low))
         [low (f low) high (f high)]
         (let [middle (int (+ low (/ (- high low) 2)))]
           (if (< (f middle) target)
             (recur middle high)
             (recur low middle))))))))

(defn max-fuel
  [recipes]
  (let [single-fuel-ore (ore-amount recipes)
        start-guess (int (/ available-ore single-fuel-ore))
        end-guess (int (* 1.3 start-guess))]
    (first (binary-search (partial ore-amount recipes) available-ore start-guess end-guess))))

(defn day14-part2-soln
  []
  (max-fuel day14-input))


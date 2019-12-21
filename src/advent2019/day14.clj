(ns advent2019.day14
  (:require [clojure.string :as str]
            [advent2019.utils :as u]))

"5 TFPNF, 11 MNMBX, 1 QCMJ, 13 TXPL, 1 DJNDX, 9 XZVHQ, 2 WKGVW, 2 VQPX => 8 GPKR"

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

(def d14-s1 (reactions
             ["10 ORE => 10 A"
              "1 ORE => 1 B"
              "7 A, 1 B => 1 C"
              "7 A, 1 C => 1 D"
              "7 A, 1 D => 1 E"
              "7 A, 1 E => 1 FUEL"]))

(def d14-s2 (reactions
             ["9 ORE => 2 A"
              "8 ORE => 3 B"
              "7 ORE => 5 C"
              "3 A, 4 B => 1 AB"
              "5 B, 7 C => 1 BC"
              "4 C, 1 A => 1 CA"
              "2 AB, 3 BC, 4 CA => 1 FUEL"]))

(def d14-s3 (reactions
             ["157 ORE => 5 NZVS"
              "165 ORE => 6 DCFZ"
              "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
              "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
              "179 ORE => 7 PSHF"
              "177 ORE => 5 HKGWZ"
              "7 DCFZ, 7 PSHF => 2 XJWVT"
              "165 ORE => 2 GPVTF"
              "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]))

; (defn requirements
;   [reactions cumqty component]
;   (let [{:keys [qty comps]} (get reactions component)]
;     (println (key (ffirst comps)) component)
;     (if (= 1 (count comps))
;       (let [[k v] (ffirst comps)]
;         (if (= :ORE k)
;           [{{component v} (* cumqty qty)}]
;           (requirements reactions (* cumqty qty v) {k v})))
;       (mapcat #(requirements reactions (* cumqty qty (val (first %))) (key (first %))) comps))))

; (defn ore-amount
;   [reactions]
;   (let [reqs (requirements reactions 1 :FUEL)
;         amts (->> reqs
;                   (group-by ffirst)
;                   (u/fmap #(reduce + (map (comp second first) %)))
;                   (u/fmap #(Math/ceil %)))]
;     (println amts)
;     (int (reduce + (map (fn [[k v]] (* (val (first k)) v)) amts)))))

(declare fulfill-requirements)

(defn fulfill-requirement
  [reactions inventory claimed min-qty [chemical qty]]
  (println min-qty chemical qty)
  (println "BEFORE" chemical)
  (println @inventory)
  (println @claimed)
  (let [current (get @claimed chemical)
        demand (* min-qty (int (Math/ceil (/ (- qty current) min-qty))))
        remaining (- demand current)]
    (if (= :ORE chemical)
      (do
        (swap! inventory assoc chemical demand)
        (swap! claimed assoc chemical (- demand current))

        demand)
      (let [below (reduce + (fulfill-requirements reactions inventory claimed chemical))]
        (swap! inventory assoc chemical below)
        (swap! claimed assoc chemical (- demand current))
        below)))
  (println "AFTER")
  (println @inventory)
  (println @claimed))

(defn fulfill-requirements
  [reactions inventory claimed chemical]
  (let [{:keys [min-qty comps]} (get reactions chemical)]
    (map (partial fulfill-requirement reactions inventory claimed min-qty) comps)))

(defn multiply
  [factor ingredients]
  (mapcat (fn [[chem qty]] [chem (* factor qty)]) (partition 2 ingredients)))

(defn consume
  [inventory used [chemical qty]]
  (swap! used update chemical #(+ % qty))
  (swap! inventory update chemical #(- % qty)))

(defn take-or-make
  [recipes inventory used chemical qty]
  (let [{:keys [min-qty comps]} (get recipes chemical)
        available (get @inventory chemical)
        multiplier (int (Math/ceil (/ qty min-qty)))
        needs (multiply multiplier comps)]
    (if (> available qty)
      (consume inventory used [chemical qty])
      (do
        (doseq [comp (partition 2 needs)]
          (consume inventory used comp))))))

(defn order-if-needed
  [recipes inventory used [chemical qty]]
  (if (= :ORE chemical)
    (swap! inventory update chemical #(+ % qty))
    (let [{:keys [min-qty comps]} (get recipes chemical)
          available (get @inventory chemical)
          order-amount (int (Math/ceil (/ (- qty available) min-qty)))
          multiplier (int (Math/ceil (/ qty min-qty)))
          orders (multiply order-amount comps)]
      (doseq [[x q] (partition 2 orders)]
        (when (> q (get @inventory x))
          (order-if-needed recipes inventory used [x q]))
        (consume inventory used [x q]))
      (swap! inventory update chemical #(+ % (* multiplier min-qty))))))

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
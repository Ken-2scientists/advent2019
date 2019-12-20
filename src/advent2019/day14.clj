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
        components (map parse-component lhs-split)
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

(defn empty-state
  [reactions]
  (zipmap (conj (keys reactions) :ORE) (repeat 0)))

(defn ore-amount
  [reactions]
  (let [inventory (atom (empty-state reactions))
        claimed (atom (empty-state reactions))]
    (fulfill-requirements reactions inventory claimed :C)))
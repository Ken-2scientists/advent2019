(ns advent2019.lib.math)

(defn manhattan
  "Computes the Manhattan distance between two points"
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- y2 y1)) (Math/abs (- x2 x1))))

(defn gcd
  "Greatest common divisor between a and b"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  "Least common multiple of a and b"
  ([a b]
   (/ (* a b) (gcd a b)))
  ([a b & others]
   (reduce lcm (lcm a b) others)))

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
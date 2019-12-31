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
  [m a]
  (loop [t 0 next-t 1 r m next-r a]
    (if (zero? next-r)
      (if (neg? t) (+ t m) t)
      (let [q (quot r next-r)]
        (recur next-t (- t (* q next-t)) next-r (- r (* q next-r)))))))

(defn mod-add
  "Computes a + b mod m"
  [m a b]
  (mod (+' a b) m))

(defn mod-sub
  "Computes a - b mod m"
  [m a b]
  (mod (-' a b) m))

(defn mod-mul
  "Computes a * b mod m"
  [m a b]
  (mod (*' a b) m))

(defn mod-div
  "Computes a / b mod m
   Note that b^-1 does not always exist for all b for all m (b and m must be co-prime)"
  [m a b]
  (if (= (mod a m) (mod b m))
    1
    (mod-mul m a (mod-inverse m b))))

(defn mod-pow
  "Computes a^n mod m"
  [m a n]
  (reduce (partial mod-mul m) (repeat n a)))

(defn mod-pow-fast
  "Computes a^n mod m"
  [m a n]
  (let [bitmask (reverse (map (comp read-string str) (Long/toBinaryString n)))
        powers (take (count bitmask) (iterate (fn [x] (mod-mul m x x)) a))
        terms (for [[x bit] (map vector powers bitmask) :when (= bit 1)] x)]
    (reduce (partial mod-mul m) terms)))

(defn mod-linear-comp
  "Takes two linear functions of the form f(x) = a1*x + b1 and g(x) = a2*x + b2 (both modulo m), and
   determines the coefficients of the composite function (g(f(x)))"
  ([_ x]
   x)
  ([m [a2 b2] [a1 b1]]
   [(mod-mul m a2 a1) (mod (+' b2 (*' a2 b1)) m)]))

(defn mod-linear-inverse
  "Coefficients of the inverse of a linear function of the form (f(x) = a*x + b modulo m"
  [m [a b]]
  (let [ainv (mod-inverse m a)]
    [ainv (mod-sub m 0 (mod-mul m b ainv))]))

(defn mod-geometric-sum
  "The sum of the geometric sequence 1 + a + a^2 + ... + a^n, with all operations modulo m
   Formula from https://stackoverflow.com/questions/42032824/geometric-series-modulus-operation"
  [m a n]
  (if (zero? n)
    1
    (let [a2 (mod-mul m a a)]
      (if (even? n)
        (mod-add m 1 (mod-mul m (mod-add m a a2) (mod-geometric-sum m a2 (/ (- n 2) 2))))
        (mod-mul m (mod-add m 1 a) (mod-geometric-sum m a2 (/ (- n 1) 2)))))))

(defn mod-linear-pow
  "Determines the coefficients of the a linear function composed on itself multiple times, i.e.,
   if f(x) = a*x + b, determines f^n(x) = f(f(f(f...(f(x))))) with n nestings
   
   [a b]^n = [a^n (a^(n-1) + a^(n-2) + ... + a^1 + 1)b]
   
   Should be equivalent to (first (drop n (iterate (partial mod-linear-comp m) [a b])))"
  [m n [a b]]
  [(mod-pow-fast m a n) (mod-mul m b (mod-geometric-sum m a (dec n)))])
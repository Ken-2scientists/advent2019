(ns advent2019.day23
  (:require [manifold.stream :as s]
            [manifold.deferred :as d]
            [advent2019.intcode :as intcode]
            [advent2019.utils :as u]))

(def day23-input (u/puzzle-input-vec "day23-input.txt"))

(defn computer
  [intcode in out]
  (d/future (intcode/intcode-ex-async intcode in out)))

(defn enqueue
  [queues [idx out]]
  (let [val @(s/try-take! out :nope 1 :nope)]
    (if (= val :nope)
      queues
      (update queues idx conj val))))

(defn dequeue
  [ins queues [idx queue]]
  (println "DEQUEUE: " idx queue)
  (let [[address x y] (take 3 queue)
        chan (nth ins address)]
    (s/put! chan x)
    (s/put! chan y)
    (update queues idx (comp vec (partial drop 3)))))

(defn send-no-data
  [ins]
  (doseq [chan ins]
    (s/put! chan -1)))

(defn ready-to-send?
  [[_ queue]]
  (> (count queue) 2))

(defn stop?
  [readies]
  (some #(= 255 %) (map first (vals readies))))

(defn switch-step
  [ins outs queues]
  (let [new-queues (reduce enqueue queues (map-indexed vector outs))
        readies (sort-by first (filter ready-to-send? new-queues))]
    (send-no-data ins)
    (if (stop? readies)
      (do (println "Really?")
          [new-queues readies])
      [(reduce (partial dequeue ins) new-queues readies) nil])))

(defn network
  [intcode]
  (let [ins (for [_ (range 50)] (s/stream))
        outs (for [_ (range 50)] (s/stream))
        queues (zipmap (range 50) (repeat []))
        stepper (partial switch-step ins outs)
        _ (doall (map s/put! ins (range 50)))
        _ (send-no-data ins)
        _ (doall (map (partial computer intcode) ins outs))]
    (loop [q queues status nil]
      (if (some? status)
        status
        (let [[newqueues stat] (stepper q)]
          (recur newqueues stat))))))
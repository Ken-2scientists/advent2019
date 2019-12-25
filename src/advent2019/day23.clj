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

(defn send-packet
  [ins address [x y]]
  (let [chan (nth ins address)]
    (s/put! chan x)
    (s/put! chan y)))

(defn dequeue
  [ins {:keys [queues nat] :as state} [idx queue]]
  (let [address (first queue)
        packet (take 2 (rest queue))
        newnat (if (= address 255)
                 packet
                 (do
                   (send-packet ins address packet)
                   nat))]
    (assoc state
           :nat newnat
           :queues (update queues idx (comp vec (partial drop 3))))))

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

(defn idle-network?
  [outs queues]
  (let [queued (reduce + (map count (vals queues)))
        outs (reduce + (map (comp :pending-puts s/description) outs))]
    (= 0 (+ queued outs))))

(defn switch-step
  [ins outs {:keys [queues nat] :as state}]
  (let [new-queues (reduce enqueue queues (map-indexed vector outs))
        new-state (assoc state :queues new-queues)
        readies (filter ready-to-send? new-queues)]
    (when (idle-network? outs queues)
      (do
        (println "Packet from NAT" nat)
        (send-packet ins 0 nat)))
    (if false
      (assoc new-state :status readies)
      (reduce (partial dequeue ins) new-state readies))))

(defn network
  [intcode]
  (let [ins (for [_ (range 50)] (s/stream))
        outs (for [_ (range 50)] (s/stream))
        queues (zipmap (range 50) (repeat []))
        stepper (partial switch-step ins outs)
        _ (doall (map s/put! ins (range 50)))
        _ (send-no-data ins)
        _ (doall (map (partial computer intcode) ins outs))]
    (loop [state {:queues queues :nat [] :status nil}]
      (if (some? (state :status))
        (state :status)
        (recur (stepper state))))))
(ns advent2019.day13
  (:require [lanterna.screen :as scr]
            [manifold.deferred :as d]
            [manifold.stream :as s]
            [advent2019.lib.intcode :as intcode]
            [advent2019.lib.utils :as u]))

(def day13-input (u/puzzle-input-vec "day13-input.txt"))

(defn day13-part1-soln
  []
  (let [board (intcode/read-output (intcode/intcode-ex day13-input []))
        tile-values (flatten (partition 1 3 (drop 2 board)))]
    (get (frequencies tile-values) 2)))

(defn val->str
  [val]
  (case val
    0 " "
    1 "#"
    2 "."
    3 "="
    4 "*"))

(defn input->str
  [key]
  (case key
    :left -1
    :right 1
    0))

(defn process-update
  [screen [x y v]]
  (if (neg? x)
    (scr/put-string screen 50 0 (str "Score: " v))
    (scr/put-string screen x y (val->str v))))

(defn location-of
  [updates item]
  (first (filter #(= item (last %)) updates)))

(defn move-paddle
  [ball-loc paddle-loc]
  (if (or (nil? ball-loc) (nil? paddle-loc))
    0
    (cond
      (< ball-loc paddle-loc) -1
      (> ball-loc paddle-loc) 1
      (= ball-loc paddle-loc) 0)))

(defn update-screen
  [screen in out paddle-loc]
  (let [updates (partition 3 (s/stream->seq out 5))
        ball-loc (first (location-of updates 4))
        move (move-paddle ball-loc @paddle-loc)]
    (swap! paddle-loc (partial + move))
    (doseq [update updates] (process-update screen update))
    (scr/redraw screen)
    (s/put! in move)))

(defn breakout
  []
  (let [in (s/stream)
        out (s/stream)
        screen (scr/get-screen :swing)
        code (assoc day13-input 0 2)
        program (d/future (intcode/intcode-ex-async code in out))
        paddle-loc (atom 20)]
    (scr/start screen)
    (scr/put-string screen 50 0 (str "Score: 0"))
    (while (not (realized? program))
      (update-screen screen in out paddle-loc))
    @program))

(defn print-board
  []
  (let [board (intcode/read-output (intcode/intcode-ex day13-input []))
        tile-values (flatten (partition 1 3 (drop 2 board)))]
    (u/pprint-image tile-values 41)))

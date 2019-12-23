(ns advent2019.day15
  (:require [lanterna.screen :as scr]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [advent2019.intcode :as intcode]
            [advent2019.utils :as u]))

(def day15-input (u/puzzle-input-vec "day15-input.txt"))

(def dir->code
  {:north 1
   :south 2
   :west 3
   :east 4})

(def next-left
  {:north :west
   :west :south
   :south :east
   :east :north})

(def status
  {0 :wall
   1 :open
   2 :oxygen})

(defn tried-position
  [[x y] direction]
  (case direction
    :north [x (dec y)]
    :south [x (inc y)]
    :east [(inc x) y]
    :west [(dec x) y]))

(defn val->str
  [val]
  (case val
    :open "."
    :wall "#"
    :oxygen "*"))

(defn process-update
  [screen [[x y] v]]
  (if (neg? x)
    (scr/put-string screen 50 0 (str "Score: " v))
    (scr/put-string screen x y (val->str v))))

(defn update-screen
  [screen {:keys [room position]}]
  (doseq [[[x y] val] room]
    (scr/put-string screen (+ x 40) (+ y 21) (val->str val)))
  (scr/put-string screen (+ (first position) 40) (+ (second position) 21) "D")
  (scr/redraw screen))

(defn update-roommap
  [screen {:keys [room position direction] :as state} result]
  ; (println room position direction result (tried-position position direction))
  (let [tested-pos (tried-position position direction)
        new-room (assoc room tested-pos result)
        new-state (case result
                    :wall (merge state {:room new-room})
                    :open (merge state {:room new-room} {:position tested-pos})
                    :oxygen (merge state {:room new-room} {:position tested-pos})
                    (do (println result) state))]
    (update-screen screen new-state)
    new-state))

(defn key->move
  [key]
  (case key
    :up :north
    :down :south
    :left :west
    :right :east
    :north))

; (defn next-move
;   [{:keys [room position direction] :as state}]
;   (let [dir :north]
;     (dirs dir)))

(defn droid-step
  [in out screen state]
  (let [dir (key->move (scr/get-key-blocking screen))
        _ (s/put! in (dir->code dir))
        result (status @(s/try-take! out 5))]
    (update-roommap screen (assoc state :direction dir) result)))

(defn droid
  [intcode]
  (let [in (s/stream)
        out (s/stream)
        screen (scr/get-screen :swing)
        stepper (partial droid-step in out screen)
        program (d/future (intcode/intcode-ex-async intcode in out))]
    (scr/start screen)
    (loop [state {:room {[0,0] :open} :position [0,0] :direction :north}]
      (if (realized? program)
        state
        (recur (stepper state))))))



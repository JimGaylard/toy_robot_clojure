(ns toy-robot.core
  (:gen-class))

(def directions
  {:NORTH [0 1]
   :EAST [1 0]
   :SOUTH [0 -1]
   :WEST [-1 0]})

(def compass
  [:NORTH :EAST :SOUTH :WEST])

(defn create-robot
  "Returns a robot object with coordinates and facing"
  [coordinates facing]
  (let [[x y] coordinates]
  {:coordinates {:x x :y y} :facing facing}))

(defn place
  "Function that takes an x and y coordinate plus a facing to place the robot"
  [x y facing]
  (create-robot [x y] facing))

(defn move
  "Moves the robot forward one block in the current facing direction"
  [robot]
  (let [{{x :x y :y} :coordinates
        facing :facing} robot]
    (create-robot
      (map + [x y] (directions facing))
      facing)))

(defn turn
  "Turn the robot left or right and return a robot with the new direction"
  [robot direction]
  (let [{facing :facing} robot
        turning (direction {:left -1 :right 1})]
    (assoc robot :facing
                (first (rotate
                  turning
                  (rotate (.indexOf compass facing)
                         compass))))))

(defn rotate
  "Rotates a sequence left (negative) or right (positive) by the magnitude
  provided."
  [n s]
  (let [c (count s)]
    (take c (drop (mod n c) (cycle s)))))

(defn validate
  "Validates that the operation is allowed"
  [robot board]
  (let [{{x :x y :y} :coordinates} robot
        [x-size y-size] board]
    (and (< x x-size) (< y y-size))))

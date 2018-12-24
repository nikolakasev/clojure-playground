(ns advent-of-code-2018.dec23
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.string :as str]))

(last (mapcat identity {:x 1 :y 2}))

(defn f [& {:keys [x y]}] (+ x y))

(apply f (mapcat identity {:x 1 :y 2}))

(defn manhattan-distance-3d
  "Calculates the Manhattan distance between two coordinates"
  [[from-x
    from-y
    from-z]
   [to-x
    to-y
    to-z]]
  (+ (math/abs (- from-x to-x))
     (math/abs (- from-y to-y))
     (math/abs (- from-z to-z))))

(manhattan-distance-3d [0 0 0] [0 5 0])

(re-matches #"pos=<(\-?\d*)\,(\-?\d*)\,(\-?\d*)>\,\sr=(\d*)" "pos=<-10,-12,-20>, r=4")

(def input-small (str/split-lines "pos=<0,0,0>, r=4
pos=<1,0,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<1,3,1>, r=1"))

(defn match-to-nanobot
  [[_ x y z radius]]
  {:x (read-string x) :y (read-string y) :z (read-string z) :radius (read-string radius)})

(map (comp match-to-nanobot
           (partial re-matches #"pos=<(\-?\d*)\,(\-?\d*)\,(\-?\d*)>\,\sr=(\d*)")) input-small)

(def nanobots (sort-by :radius (map (comp match-to-nanobot
                                          (partial re-matches
                                                   #"pos=<(\-?\d*)\,(\-?\d*)\,(\-?\d*)>\,\sr=(\d*)")) input-small)))

(last nanobots)
(rest nanobots)

(def beacon (last nanobots))

(filter #(<= % (:radius beacon)) (map #((partial manhattan-distance-3d [(:x beacon) (:y beacon) (:z beacon)]) [(:x %) (:y %) (:z %)])
                                      nanobots))

(def input (str/split-lines (slurp "src/advent_of_code_2018/input-dec23.txt")))

(def nanobots (sort-by :radius (map (comp match-to-nanobot
                                          (partial re-matches
                                                   #"pos=<(\-?\d*)\,(\-?\d*)\,(\-?\d*)>\,\sr=(\d*)")) input)))

(count nanobots)
(def beacon (last nanobots))

;solved P1, 497
(count (filter #(<= % (:radius beacon)) (map #((partial manhattan-distance-3d [(:x beacon) (:y beacon) (:z beacon)]) [(:x %) (:y %) (:z %)])
                                             nanobots)))

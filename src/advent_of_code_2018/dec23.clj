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
  [[_ x y z range]]
  {:x (read-string x) :y (read-string y) :z (read-string z) :range (read-string range)})

(def input (str/split-lines (slurp "src/advent_of_code_2018/input-dec23.txt")))

(def nanobots (sort-by :range (map (comp match-to-nanobot
                                         (partial re-matches
                                                  #"pos=<(\-?\d*)\,(\-?\d*)\,(\-?\d*)>\,\sr=(\d*)")) input)))

(count nanobots)
(def beacon (last nanobots))

;solved P1, 497
(count (filter #(<= % (:range beacon)) (map #((partial manhattan-distance-3d [(:x beacon) (:y beacon) (:z beacon)]) [(:x %) (:y %) (:z %)])
                                            nanobots)))

(def input-small-p2 (str/split-lines "pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5"))

(def nanobots (sort-by :range (map (comp match-to-nanobot
                                         (partial re-matches
                                                  #"pos=<(\-?\d*)\,(\-?\d*)\,(\-?\d*)>\,\sr=(\d*)")) input)))

(first nanobots)

(:x (first (sort-by :x nanobots)))
(:x (last (sort-by :x nanobots)))
(:y (first (sort-by :y nanobots)))
(:y (last (sort-by :y nanobots)))
(:z (first (sort-by :z nanobots)))
(:z (last (sort-by :z nanobots)))

(apply + (map (fn [bot] (if (>= (:range bot) ((partial manhattan-distance-3d [12 12 12]) [(:x bot) (:y bot) (:z bot)])) 1 0)) nanobots))

(str/join (map #(str "|" (:x %) ", " (:y %) ", " (:z %)) (sort-by :range nanobots)))

(last nanobots)

(defn write-area-to-file
  [area
   filename]
  (with-open [w (clojure.java.io/writer filename)]
    (doseq [line area]
      (.write w line)
      (.newLine w))))

(write-area-to-file (map #(str "|" (:x %) ", " (:y %) ", " (:z %)) (sort-by :range nanobots)) "src/advent_of_code_2018/out-bots.txt")

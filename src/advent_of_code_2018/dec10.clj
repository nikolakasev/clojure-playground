(ns advent-of-code-2018.dec10
  (:require [clojure.string :as str])
  (:require [clojure.math.numeric-tower :as math]))

(def regex #"-?\d+")

(defn line-to-point
  [line]
  (zipmap [:p-x :p-y :v-x :v-y] (map read-string (re-seq regex line))))

(def input-small "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>")

(def points-small (map line-to-point (str/split-lines input-small)))

(count points-small)

(first points-small)

(defn fast-forward-point
  [seconds
   point]
  (-> point
      (update :p-x + (* (:v-x point) seconds))
      (update :p-y + (* (:v-y point) seconds))))

(fast-forward-point 10 (first points-small))

(apply min [4 1 2 -1])

(defn span-in-future
  [points
   seconds]
  (let [points-fast-forward (map (partial fast-forward-point seconds) points)
        min-x (apply min (map :p-x points-fast-forward))
        max-x (apply max (map :p-x points-fast-forward))
        min-y (apply min (map :p-y points-fast-forward))
        max-y (apply max (map :p-y points-fast-forward))]
    (* (math/abs (- max-x min-x))
       (math/abs (- max-y min-y)))))

(def φ (/ (+ 1 (math/sqrt 5)) 2))
(def φ2 (/ (+ 1 (math/sqrt 5)) 2))

(defn gss
  "Golden section search to find the minimum of a function `f` on [a, b] with tolerance `tol`."
  [f a b tol]
  (let [φ (/ (- (math/sqrt 5) 1) 2)
        φ2 (/ (- 3 (math/sqrt 5)) 2)]
    (loop [f f
           a a
           b b
           tol tol
           h (- b a)]
      (if (<= (math/abs h) tol)
        [a b]
        (let [c (+ a (* φ2 h))
              fc (f c)
              d (+ a (* φ h))
              fd (f d)]
          (if (< fc fd)
            (recur f a d tol (* h φ))
            (recur f c b tol (* h φ))))))))

(gss (fn [x] (math/expt (- x 2) 2)) 1 5 0.1)

(math/expt 2 3)

;assume the message will show up between the first and tenth second
;[2.978260659894007 3.0514362286960575]
(gss (partial span-in-future points-small) 1 10 0.1)

(def input (slurp "src/advent_of_code_2018/input-dec10.txt"))
(def points (map line-to-point (str/split-lines input)))

(count points)

;[10885.963304973997 10886.03698158382], so it's the 10886-th second
(gss (partial span-in-future points) 1 20000 0.1)

;proudly copying from https://github.com/mfikes/advent-of-code/blob/master/src/advent_2018/day_10.cljc, thank you!
(defn raster-geometry [points]
  (let [min-max (juxt #(apply min %) #(apply max %))
        [min-x max-x] (min-max (map :p-x points))
        [min-y max-y] (min-max (map :p-y points))]
    [min-x min-y (- max-x min-x) (- max-y min-y)]))

(defn rasterize [points]
  (let [[origin-x origin-y width height] (raster-geometry points)]
    (reduce (fn [raster {:keys [p-x p-y]}]
              (assoc-in raster [(- p-y origin-y) (- p-x origin-x)] "#"))
            (vec (repeat (inc height) (vec (repeat (inc width) "."))))
            points)))

(defn print-raster [raster]
  (println (str/join \newline (map str/join raster))))

(print-raster (rasterize (map (partial fast-forward-point 10886) points)))

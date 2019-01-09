(ns advent-of-code-2018.dec11)

(defn power-level
  [serial-number
   x y]
  (let [rack-id (+ x 10)
        power (* (+ (* rack-id y) serial-number) rack-id)]
    (if (< power 100)
      -5
      (- (Character/digit (nth (reverse (str power)) 2) 10) 5))))

(= 4 (power-level 8 3 5))
(= -5 (power-level 57 122 79))
(= 0 (power-level 39 217 196))
(= 4 (power-level 71 101 153))

(defn grid-to-power-map
  [max-x
   max-y
   serial-number]
  (reduce (fn [power [x y]]
            (assoc-in power [x y] (power-level serial-number x y)))
          (sorted-map)
          (for [y (range 1 (+ max-y 1))
                x (range 1 (+ max-x 1))]
            [x y])))

(grid-to-power-map 5 5 8)

(defn any-square-power
  [power-map
   max-x
   max-y
   square-size]
  (do (println "size: " square-size)
      (last (sort-by last (for [y (range 1 (- max-y (- square-size 2)))
                                x (range 1 (- max-x (- square-size 2)))]
                            [x y square-size (apply + (map (partial get-in power-map)
                                                           (for [square-y (range y (+ y square-size))
                                                                 square-x (range x (+ x square-size))]
                                                             [square-x square-y])))])))))

(any-square-power (grid-to-power-map 300 300 18) 300 300 3)
(any-square-power (grid-to-power-map 300 300 42) 300 300 3)

(into (sorted-map) [[0 0]])

;x:235 y:18 with a power level of 31 solves P1
(any-square-power (grid-to-power-map 300 300 5153) 300 300 3)

(any-square-power (grid-to-power-map 300 300 18) 300 300 16)

;this is too slow
;(last (sort-by last (map (partial any-square-power (grid-to-power-map 300 300 5153) 300 300) (range 2 300))))

(get-in (grid-to-power-map 10 10 5153) [1 0] 0)

(defn summed-area-table-from-power-map
  [power-map
   max-x
   max-y]
  (reduce (fn [summed-area-table [x y]]
            (let [value-on-left (get-in summed-area-table [x (- y 1)] 0)
                  value-above (get-in summed-area-table [(- x 1) y] 0)
                  value-top-left-diagonal (get-in summed-area-table [(- x 1) (- y 1)] 0)
                  value (-> (get-in power-map [x y] 0)
                            (+ value-on-left)
                            (+ value-above)
                            (- value-top-left-diagonal))]
              (assoc-in summed-area-table [x y] value)))
          (sorted-map)
          (for [x (range 1 (+ max-x 1))
                y (range 1 (+ max-y 1))]
            [x y])))

(summed-area-table-from-power-map (grid-to-power-map 10 10 5153) 10 10)

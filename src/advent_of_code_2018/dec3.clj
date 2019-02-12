(ns advent-of-code-2018.dec3
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.math.numeric-tower :as math]))

(def id "#1 @ 1,3: 4x4")
(def id-2 "#2 @ 3,1: 4x4")
(def id-3 "#3 @ 5,5: 2x2")

(re-matches #"#\d*\s@\s(\d*)\,(\d*):\s(\d*)x(\d*)" id)
(re-matches #"#\d*\s@\s(\d*)\,(\d*):\s(\d*)x(\d*)" "#13 @ 68,980: 11x19")

(System/getProperty "user.dir")
(slurp "src/advent_of_code_2018/input-dec3.txt")

(defn box-coordinates
  [box-id]
  (let [groups (rest (re-matches #"#\d*\s@\s(\d*)\,(\d*):\s(\d*)x(\d*)" box-id))
        c (read-string (second groups))
        r (read-string (first groups))
        tall (read-string (first (reverse groups)))
        wide (read-string (second (reverse groups)))]
    [[r c] [r (+ c wide)] [(+ r tall) c] [(+ r tall) (+ c wide)]]))

(defn fabric-span
  [claim-id]
  (let [groups (rest (re-matches #"#\d*\s@\s(\d*)\,(\d*):\s(\d*)x(\d*)" claim-id))
        x (read-string (first groups))
        y (read-string (second groups))
        tall (read-string (first (reverse groups)))
        wide (read-string (second (reverse groups)))]
    [x y wide tall]))

(box-coordinates "#13 @ 68,980: 11x19")
(fabric-span "#13 @ 68,980: 11x19")

(box-coordinates id)
(box-coordinates id-2)
(nth (box-coordinates id-3) 0)

(>= 2 4)

(math/abs -2)

(defn overlapping-area
  [ur1
   ll1
   ur2
   ll2]
  (* (max 0 (- (min (first ll1) (first ll2))
               (max (first ur1) (first ur2))))
     (max 0 (- (min (second ur1) (second ur2))
               (max (second ll1) (second ll2))))))

(overlapping-area (second (box-coordinates id)) (nth (box-coordinates id) 2) (second (box-coordinates id-2)) (nth (box-coordinates id-2) 2))

(overlapping-area (second (box-coordinates id-2)) (nth (box-coordinates id-2) 2) (second (box-coordinates id-3)) (nth (box-coordinates id-3) 2))

(min 5 0)

;the overlap between box 2 and 3 is 4 which is correct
(apply + (map (fn [[one two]] (overlapping-area (second (box-coordinates one)) (nth (box-coordinates one) 2) (second (box-coordinates two)) (nth (box-coordinates two) 2))) (combo/combinations [id id-2] 2)))

;understood the puzzle wrong... calculates an overlap for each unique pair and sum up for a total of 186644 which is too high...
(apply + (map (fn [[one two]] (overlapping-area (second (box-coordinates one)) (nth (box-coordinates one) 2) (second (box-coordinates two)) (nth (box-coordinates two) 2))) (combo/combinations (clojure.string/split-lines (slurp "src/advent_of_code_2018/input-dec3.txt")) 2)))

;alternative of (frequencies list)
(count (filter (fn [[_ list]] (> (count list) 1))
               (group-by identity [[1 2] [1 2] [3 4] [5 6] [1 2] [5 6] [5 7]])))

;f(x) = x
(identity [1 2])

(defn area-of-fabric
  [claim]
  ;en example of destructuring the result of the function fabric-span
  (let [[b-x b-y wide tall] (fabric-span claim)]
    (for [x (range b-x (+ b-x wide))
          y (range b-y (+ b-y tall))]
      [x y])))

;@jumar proposes:
(defn to-matrix
  [row matrix-range]
  (for [dx matrix-range
        [x y] row]
    [(+ x dx) y]))

(to-matrix [[1 2] [1 3] [1 4]] (range 0 4))

(area-of-fabric id)

(def input-small (clojure.string/split-lines "#1 @ 387,801: 11x22
#2 @ 101,301: 19x14
#3 @ 472,755: 11x16"))

(def input
  (clojure.string/split-lines (slurp "src/advent_of_code_2018/input-dec3.txt")))

(contains? #{[1 2] [3 4] [5 6]} [3 4])

(defn doesnt-overlap
  [overlap-square-inches
   claim]
  (if (every? false? (map
                      (fn [square-inch] (contains? overlap-square-inches square-inch))
                      (area-of-fabric claim)))
    claim))

(def overlaps
  (filter (fn [[_ list]] (> (count list) 1)) (group-by identity (mapcat area-of-fabric input))))

;115304 - solved D3P1
(count overlaps)

(first overlaps)

;because of the group-by
(def overlaps-keys
  (map (fn [[i _]] i) overlaps))

(first overlaps-keys)

;solved D3P2
(filter (comp not nil?) (map (partial doesnt-overlap (set overlaps-keys)) input))

(ns advent-of-code-2018.dec6
  (:require [clojure.math.numeric-tower :as math]))

(def grid "aaaaa.cccc
aAaaa.cccc
aaaddecccc
aadddeccCc
..dDdeeccc
bb.deEeecc
bBb.eeee..
bbb.eeefff
bbb.eeffff
bbb.ffffFf")

(def grid-lines
  (clojure.string/split-lines (clojure.string/lower-case grid)))

(defn vertical-border
  [grid-lines]
  ;take the first and last character for each line, this makes the left and right borders
  (clojure.string/join (map #(str (first (vec %)) (last (vec %))) grid-lines)))

(defn infinite-locations
  [grid-lines]
  (let [frame (str (first grid-lines) (last grid-lines) (vertical-border grid-lines))]
    (set (map (fn [[k _]] k) (frequencies frame)))))

((comp not contains?) (infinite-locations grid-lines) \a)

;finite areas with their sizes
(filter (fn [[area _]] ((comp not contains?) (infinite-locations grid-lines) area)) (frequencies (clojure.string/join grid-lines)))

;this can wait
(sort (fn [[_ int]] int) ([\d 9] [\e 17]))

(defn manhattan-distance
  [from-x
   from-y
   area-x
   area-y]
  (+ (math/abs (- from-x area-x))
     (math/abs (- from-y area-y))))

(manhattan-distance 2 2 3 4)

(def input
  (map
   ;extract the coordinates and turn into int
   (fn [[_ x y]] [(read-string x) (read-string y)])
   (map
    ;reg ex each line
    (partial re-matches #"(\d*),\s(\d*)")
    (clojure.string/split-lines (slurp "src/advent_of_code_2018/input-dec6.txt")))))

;max 355
(last (sort (flatten input)))

;max on x = 352
(last (sort (map (fn [[x _]] x) input)))

;max on y = 355
(last (sort (map (fn [[_ y]] y) input)))

(flatten [[1 2] [3 4]])

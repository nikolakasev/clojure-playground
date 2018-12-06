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

(vertical-border grid-lines)

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

(defn input-to-coordinates
  [input]
  (map
   ;extract the coordinates and turn into int
   (fn [[_ x y]] [(read-string x) (read-string y)])
   (map
    ;reg ex each line
    (partial re-matches #"(\d*),\s(\d*)")
    (clojure.string/split-lines input))))

(def input
  (input-to-coordinates (slurp "src/advent_of_code_2018/input-dec6.txt")))

(def input-small
  (input-to-coordinates "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"))

;max on x = 352
(last (sort (map (fn [[x _]] x) input)))

;max on y = 355
(last (sort (map (fn [[_ y]] y) input)))

(flatten [[1 2] [3 4]])

(last input-small)

(def area
  (for [y (range 0 (+ 1 (last (sort (map (fn [[_ y]] y) input-small)))))
        x (range 0 (+ 1 (last (sort (map (fn [[x _]] x) input-small)))))]
    [x y]))

(def loc
  (:locations {:coordinates [0 0] :locations [["a" 3] ["b" 1] ["c" 2]]}))

(defn closest-location
  ;TODO is this a good name?
  [locations] ;[["a" 3] ["b" 1] ["c" 2]] returns b
  (let [distances (sort-by last locations)]
    (if (= (last (first distances))
           (last (second distances)))
      \.
      (first (first distances)))))

(closest-location loc)

(defn closest-location2
  [locations
   [x y]]
  (closest-location (map (fn [[name location]]
                           [name ((partial manhattan-distance x y) (first location) (second location))])
                         locations)))

(first [:a 1])

;guarantee unique name for an area
(def names "abcdefghijklmnopqrstuvwxyzабвгдежзийклмнопрстуфхцч")

(def names-small "abcdef")

(def wieve-input-small
  (zipmap (vec names-small) input-small))

(def wieve-input
  (zipmap (vec names) input))

(last wieve-input-small)
(count input)

(closest-location2 wieve-input-small [8 9])

(map (partial closest-location2 wieve-input-small) area)

;left and right vertical borders
;(seq (clojure.string/join (map #(str (first %) (last %)) (partition 9 aha))))

(concat (take 9 aha) (take 9 (reverse aha)) (seq (clojure.string/join (map #(str (first %) (last %)) (partition 9 aha)))))

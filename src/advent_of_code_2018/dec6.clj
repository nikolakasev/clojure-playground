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

(defn closest-location
  "For an array of locations, determine which one is closest"
  ;TODO is this a good name?
  [locations] ;[["a" 3] ["b" 1] ["c" 2]] returns b
  (let [distances (sort-by last locations)]
    (if (= (last (first distances))
           (last (second distances)))
      \. ;two locations have the same distance, so it's a tie
      (first (first distances)))))

(closest-location [["a" 4] ["b" 2] ["c" 6]])

(defn closest-location-from
  "Determines the letter of the closest location for a given coordinate"
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

(defn largest-finite-area
  [input
   names]
  (let [max-y (last (sort (map (fn [[_ y]] y) input)))
        max-x (last (sort (map (fn [[x _]] x) input)))
        area (for [y (range 0 (+ 1 max-y))
                   x (range 0 (+ 1 max-x))]
               [x y])
        ;give a letter to each area
        wieve-input (zipmap (vec names) input)
        ;calculate a letter for the closest area in each square - generates the view with letters
        area-with-letters (map (partial closest-location2 wieve-input) area)
        infinite-areas (concat (take max-y area-with-letters)
                               (take max-y (reverse area-with-letters))
                               (seq (clojure.string/join (map #(str (first %) (last %)) (partition max-y area-with-letters)))))]
    ;(partition max-y area-with-letters)))
    (filter (fn [[area _]] ((comp not contains?) (set infinite-areas) area)) (frequencies area-with-letters))))

(map #(seq %) (largest-finite-area input-small names-small))

(last (sort-by last (largest-finite-area input names)))

(count (largest-finite-area input names))

([\р 662] [\a 2303] [\b 1118] [\у 522] [\d 3363] [\e 1166] [\i 2194] [\j 1043] [\l 1335] [\p 2329] [\б 1915] [\r 4819] [\г 867] [\u 1612] [\ж 1300] [\v 2357] [\w 725] [\и 1077] [\x 1305] [\й 1945] [\y 1051] [\z 1309] [\о 1750] [\п 1326])

{\р [127 346], \с [213 102], \a [156 193], \т [313 319], \b [81 315], \у [207 134], \c [50 197], \ф [154 253], \d [84 234], \х [50 313], \e [124 162], \ц [160 330], \f [339 345], \ч [332 163], \g [259 146], \h [240 350], \i [97 310], \j [202 119], \k [188 331], \l [199 211], \m [117 348], \n [350 169], \o [131 355], \а [89 332], \p [71 107], \б [254 181], \q [214 232], \в [113 117], \r [312 282], \г [120 161], \s [131 108], \д [322 43], \t [224 103], \е [115 226], \u [83 122], \ж [324 222], \v [352 142], \з [151 240], \w [208 203], \и [248 184], \x [319 217], \й [207 136], \y [224 207], \к [41 169], \z [327 174], \л [63 78], \м [286 43], \н [84 222], \о [81 167], \п [128 192]}

(partition 4 [1 2 3 4 5 6 7 8])

(ns advent-of-code-2018.dec6
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

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
  (str/split-lines (str/lower-case grid)))

(defn vertical-border
  [grid-lines]
  ;take the first and last character for each line, this makes the left and right borders
  (str/join (map #(str (first (vec %)) (last (vec %))) grid-lines)))

(vertical-border grid-lines)

(defn infinite-locations
  [grid-lines]
  (let [frame (str (first grid-lines) (last grid-lines) (vertical-border grid-lines))]
    (set (map (fn [[k _]] k) (frequencies frame)))))

((comp not contains?) (infinite-locations grid-lines) \a)

;finite areas with their sizes
(filter (fn [[area _]] ((comp not contains?) (infinite-locations grid-lines) area)) (frequencies (str/join grid-lines)))

;this can wait
(sort (fn [[_ int]] int) ([\d 9] [\e 17]))

(defn manhattan-distance
  "Calculates the Manhattan distance between two coordinates"
  [[from-x
    from-y]
   [to-x
    to-y]]
  (+ (math/abs (- from-x to-x))
     (math/abs (- from-y to-y))))

(manhattan-distance [2 2] [3 4])

(defn input-to-coordinates
  [input]
  (map
   ;extract the coordinates and turn into int
   (fn [[_ x y]] [(read-string x) (read-string y)])
   (map
    ;reg ex each line
    (partial re-matches #"(\d*),\s(\d*)")
    (str/split-lines input))))

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
  "For an array of locations, determine which one is closest."
  [locations] ;[["a" 3] ["b" 1] ["c" 2]] returns b
  (let [distances (sort-by last locations)]
    (if (= (last (first distances))
           (last (second distances)))
      \. ;two locations have the same distance, so it's a tie
      (first (first distances)))))

(closest-location [["a" 1] ["b" 2] ["c" 6]])

(defn closest-location-with-case
  "For an array of locations, determine which one is closest. Capital letter for a location."
  ;TODO is this a good name?
  [locations] ;[["a" 3] ["b" 1] ["c" 2]] returns b
  (let [distances (sort-by last locations)]
    (if (= 0 (last (first distances)))
      (str/upper-case (first (first distances)))
      (closest-location distances))))

(defn closest-location-from
  "Determines the letter of the closest location for a given coordinate."
  [locations
   from]
  (closest-location-with-case (map (fn [[name location]]
                                     [name ((partial manhattan-distance from) location)])
                                   locations)))

(first [:a 1])

;guarantee unique name for an area
(def names "abcdefghijklmnopqrstuvwxyzабвгдежзийклмнопрстуфхцч")

(def names-small "abcdef")

(def weave-input-small
  (zipmap (vec names-small) input-small))

(def weave-input
  (zipmap (vec names) input))

(last weave-input-small)
(count input)

(closest-location-from weave-input-small [8 8])

(closest-location-from weave-input [127 344])

{\р [127 346], \с [213 102], \a [156 193], \т [313 319], \b [81 315], \у [207 134], \c [50 197], \ф [154 253], \d [84 234], \х [50 313], \e [124 162], \ц [160 330], \f [339 345], \ч [332 163], \g [259 146], \h [240 350], \i [97 310], \j [202 119], \k [188 331], \l [199 211], \m [117 348], \n [350 169], \o [131 355], \а [89 332], \p [71 107], \б [254 181], \q [214 232], \в [113 117], \r [312 282], \г [120 161], \s [131 108], \д [322 43], \t [224 103], \е [115 226], \u [83 122], \ж [324 222], \v [352 142], \з [151 240], \w [208 203], \и [248 184], \x [319 217], \й [207 136], \y [224 207], \к [41 169], \z [327 174], \л [63 78], \м [286 43], \н [84 222], \о [81 167], \п [128 192]}

(def output
  (map #(str/join %) (partition 9 (map (partial closest-location-from weave-input-small) area))))

(with-open [w (clojure.java.io/writer "src/advent_of_code_2018/out.txt")]
  (doseq [line output]
    (.write w line)
    (.newLine w)))

;left and right vertical borders
;(seq (str/join (map #(str (first %) (last %)) (partition 9 aha))))

(defn write-area-to-file
  [area
   filename]
  (with-open [w (clojure.java.io/writer filename)]
    (doseq [line area]
      (.write w line)
      (.newLine w))))

(defn largest-finite-area
  [input
   names]
  (let [max-y (last (sort (map (fn [[_ y]] y) input)))
        max-x (last (sort (map (fn [[x _]] x) input)))
        area (for [y (range 0 (+ 1 max-y))
                   x (range 0 (+ 1 max-x))]
               [x y])
        ;give a letter to each area
        weave-input (zipmap (vec names) input)
        ;calculate a letter for the closest area in each square - generates the view with letters
        area-with-letters (map (partial closest-location-from weave-input) area)
        infinite-areas (concat (take max-x area-with-letters)
                               (take max-x (reverse area-with-letters))
                               (seq (str/join (map #(str (first %) (last %)) (partition (+ 1 max-x) area-with-letters)))))
        output (map #(str/join %) (partition (+ 1 max-x) area-with-letters))]
    (write-area-to-file output "src/advent_of_code_2018/out.txt")
    (filter (fn [[area _]] ((comp not contains?) (set infinite-areas) area)) (frequencies area-with-letters))))

(map #(seq %) (largest-finite-area input-small names-small))

;4341 + 1 for the capital letter ;), have to bring the kids to school
(last (sort-by last (largest-finite-area input names)))

(defn total-distance-to-all-locations
  [locations
   from]
  (apply + (map (fn [location] ((partial manhattan-distance from) location)) locations)))

(total-distance-to-all-locations input-small [4 2])

(defn closest-location-with-coordinates
  "Determines the letter of the closest location for a given coordinate."
  [locations
   safe-areas
   from]
  (let [groups (sort-by first (group-by second (map (fn [[name location]]
                                                      [name ((partial manhattan-distance from) location)])
                                                    locations)))
        closest-areas (second (first groups))
        safe-areas (set/intersection safe-areas (set (map first closest-areas)))]
    (if (not (empty? safe-areas))
      ;doesn't matter which on to take, they are all safe, take retu
      ;TODO the letter of the location isn't needed, just the coordinates
      [(first safe-areas) from])))

{5 [[\a 5]], 2 [[\b 2] [\d 2]], 8 [[\c 8]], 3 [[\e 3]], 10 [[\f 10]]}

(set/intersection #{\b \d} #{\d \e \b})

(set (map first [[\b 2] [\d 2]]))

(def safe-areas-small
  (set [\d \e]))

(closest-location-with-coordinates weave-input-small safe-areas-small [3 6])

(defn safe-region-under-10K
  [input
   names
   safe-areas]
  (let [max-y (last (sort (map (fn [[_ y]] y) input)))
        max-x (last (sort (map (fn [[x _]] x) input)))
        area (for [y (range 0 (+ 1 max-y))
                   x (range 0 (+ 1 max-x))]
               [x y])
        ;give a letter to each area
        weave-input (zipmap (vec names) input)
        ;extract only the coordinates of safe locations
        safe-coordinates (filter (comp not nil?) (map (partial closest-location-with-coordinates weave-input safe-areas) area))

        total-distances (map #((partial total-distance-to-all-locations input) (second %)) safe-coordinates)]
    total-distances))

[]

;safe areas
(def safe-areas
  (set (map first [[\р 662] [\a 2303] [\т 2687] [\b 1118] [\у 522] [\ф 2879] [\e 1166] [\ч 1061] [\g 3433] [\i 2194] [\j 1043] [\l 1335] [\б 1915] [\q 4342] [\г 867] [\е 1710] [\з 1277] [\w 725] [\и 1077] [\x 1305] [\й 1945] [\y 1051] [\z 1309] [\н 1130] [\о 1750] [\п 1326]])))

(def safe-areas-small
  (set [\d \e]))

;33639 isn't the right answer
(count (filter (partial > 31) (safe-region-under-10K input-small names-small safe-areas-small)))

(count (safe-region-under-10K input-small names-small safe-areas-small))

(closest-location-with-coordinates weave-input-small [3 4])

(total-distance-to-all-locations input-small [4 7])

([\a [0 0]] [\a [1 0]] [\a [2 0]] [\a [3 0]] [\a [4 0]] [\. [5 0]] [\c [6 0]] [\c [7 0]] [\c [8 0]] [\a [0 1]] [\a [1 1]] [\a [2 1]] [\a [3 1]] [\a [4 1]] [\. [5 1]] [\c [6 1]] [\c [7 1]] [\c [8 1]] [\a [0 2]] [\a [1 2]] [\a [2 2]] [\d [3 2]] [\d [4 2]] [\e [5 2]] [\c [6 2]] [\c [7 2]] [\c [8 2]] [\a [0 3]] [\a [1 3]] [\d [2 3]] [\d [3 3]] [\d [4 3]] [\e [5 3]] [\c [6 3]] [\c [7 3]] [\c [8 3]] [\. [0 4]] [\. [1 4]] [\d [2 4]] [\d [3 4]] [\d [4 4]] [\e [5 4]] [\e [6 4]] [\c [7 4]] [\c [8 4]] [\b [0 5]] [\b [1 5]] [\. [2 5]] [\d [3 5]] [\e [4 5]] [\e [5 5]] [\e [6 5]] [\e [7 5]] [\c [8 5]] [\b [0 6]] [\b [1 6]] [\b [2 6]] [\. [3 6]] [\e [4 6]] [\e [5 6]] [\e [6 6]] [\e [7 6]] [\. [8 6]] [\b [0 7]] [\b [1 7]] [\b [2 7]] [\. [3 7]] [\e [4 7]] [\e [5 7]] [\e [6 7]] [\f [7 7]] [\f [8 7]] [\b [0 8]] [\b [1 8]] [\b [2 8]] [\. [3 8]] [\e [4 8]] [\e [5 8]] [\f [6 8]] [\f [7 8]] [\f [8 8]] [\b [0 9]] [\b [1 9]] [\b [2 9]] [\. [3 9]] [\f [4 9]] [\f [5 9]] [\f [6 9]] [\f [7 9]] [\f [8 9]])

(def safe
  [[\d [3 2]] [\d [4 2]] [\e [5 2]] [\d [2 3]] [\d [3 3]] [\d [4 3]] [\e [5 3]] [\d [2 4]] [\d [3 4]] [\d [4 4]] [\e [5 4]] [\e [6 4]] [\d [3 5]] [\e [4 5]] [\e [5 5]] [\e [6 5]] [\e [7 5]] [\e [4 6]] [\e [5 6]] [\e [6 6]] [\e [7 6]] [\e [4 7]] [\e [5 7]] [\e [6 7]] [\e [4 8]] [\e [5 8]]])

(def distances [34 34 34 32 30 30 30 30 28 28 28 30 28 28 28 30 32 30 30 32 34 34 34 36 38 38])

(zipmap safe distances)

{[\e [5 2]] 34, [\d [3 2]] 34, [\e [4 6]] 30, [\d [2 3]] 32, [\d [3 5]] 28, [\e [7 6]] 34, [\d [4 4]] 28, [\e [5 8]] 38, [\e [5 5]] 28, [\e [4 7]] 34, [\e [7 5]] 32, [\e [6 6]] 32, [\e [4 5]] 28, [\d [4 3]] 30, [\e [5 4]] 28, [\d [4 2]] 34, [\e [6 7]] 36, [\e [5 3]] 30, [\e [5 6]] 30, [\e [6 4]] 30, [\d [3 3]] 30, [\e [5 7]] 34, [\e [6 5]] 30, [\d [3 4]] 28, [\d [2 4]] 30, [\e [4 8]] 38}

{[\e [4 6]] 30, [\d [3 5]] 28, [\d [4 4]] 28, [\e [5 5]] 28, [\e [4 5]] 28
 [\d [4 3]] 30, [\e [5 4]] 28, [\e [5 3]] 30, [\e [5 6]] 30, [\e [6 4]] 30
 [\d [3 3]] 30, [\e [6 5]] 30, [\d [3 4]] 28, [\d [2 4]] 30}

345 3
23456 4
23456 5
345 6

3
4
2 5
3 6

(count (second (second (group-by first safe))))

;to investigate how to work with complex maps
;(map first [{:name \d :coordinates [1 2]} {:name \a :coordinates [0 0]}])

;(filter #((partial contains? safe-areas-small) (:name (first %))) [{:name \a, :coordinates [0 0]} {:name \d, :coordinates [1 0]}])

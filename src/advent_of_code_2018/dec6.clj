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
        a-safe-area (set/intersection safe-areas (set (map first closest-areas)))]
    (if (not (empty? a-safe-area))
      ;doesn't matter which one to take, they are all safe
      ;TODO the letter of the location isn't needed, just the coordinates
      [(first a-safe-area) from])))

(set/intersection #{\b \d} #{\d \e \b})

(set (map first [[\b 2] [\d 2]]))

(def safe-areas-small
  (set [\d \e]))

(closest-location-with-coordinates weave-input-small safe-areas-small [8 6])

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

;safe areas
(def safe-areas
  (set (map first [[\р 662] [\a 2303] [\т 2687] [\b 1118] [\у 522] [\ф 2879] [\e 1166] [\ч 1061] [\g 3433] [\i 2194] [\j 1043] [\l 1335] [\б 1915] [\q 4342] [\г 867] [\е 1710] [\з 1277] [\w 725] [\и 1077] [\x 1305] [\й 1945] [\y 1051] [\z 1309] [\н 1130] [\о 1750] [\п 1326]])))

(def safe-areas-small
  (set [\d \e]))

;33639 isn't the right answer
(count (filter (partial > 31) (safe-region-under-10K input-small names-small safe-areas-small)))
(count (filter (partial > 9999) (safe-region-under-10K input names safe-areas)))

(count (safe-region-under-10K input-small names-small safe-areas-small))

(closest-location-with-coordinates weave-input-small safe-areas-small [3 4])

(total-distance-to-all-locations input-small [4 7])

;to investigate how to work with complex maps
;(map first [{:name \d :coordinates [1 2]} {:name \a :coordinates [0 0]}])

;(filter #((partial contains? safe-areas-small) (:name (first %))) [{:name \a, :coordinates [0 0]} {:name \d, :coordinates [1 0]}])

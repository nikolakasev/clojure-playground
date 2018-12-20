(ns advent-of-code-2018.dec18
  (:require [clojure.string :as str]))

(def input ".|||.#..|##.#||..#.|..|..||||..#|##.##..#...|.....
.|#.|#..##...|#.........#.#..#..|#.|#|##..#.#|..#.
#....#|.#|.###||..#.|...|.|.#........#.|.#.#|..#..
|..|#....|#|...#.#..||.#..||......#.........|....|
.|.|..#|...#.|.###.|...||.|.|..|...|#|.#..|.|..|.|
#.....||.#..|..|..||#.||#..|.||..||##.......#.....
||.#..........|....##...|..#.|..#..#|#.#....#..#.#
.#.#|.|.|.##|..#......|...#||..#.||..|..|#....|##.
#.#..||.|...#|...|..#.#.||#.||.#.|.....|##.|....#.
.#......||.|#......#|#.|...||...||##...#...####.#.
.....#..|..#..#|..#...#.|#...||...#.##.||.|..|.||.
.#|.#.|.....|#..#||..|...|...##.#.###|..|.###.|#..
..#.......#.|#.##....#..|##.#......#|......#..#...
.|..#|.#.....#..||..#.#.|##..|#.||#..|.#..|.|##|#|
##|.#........|#.#.#|..|....|.......#..#|.#.|....#.
....##...|....#..............||.|..#........|.....
##||.|.#...|.#|..#....#..|...|..#..#..|##||.....|.
.|.#...|#.......#...#.#..|#....#|#|#..#|...##..||.
.|..|.|..#...##...||#..##|#|..|...#.....#||...##..
.|...|..||#..#|.|.#...|||.|#.||#|......|#|.#..|#..
|##.....|.|#...#||.....#..#.|.#..|.....||....||..#
|.|#|||.....|||..#......#..||........||.#.#..||#||
#.|.|.#.....#....#.#..#||.||..|.#.|....|...#.#...#
|.|....#.#||...#.....#|#|.|.#......##.|.||...#.||.
|...|...|##........|.|...#...|.........|..##..|.##
|.||..|.#.#|.#||...|.|.....#...#.####|.||||..|||.|
.....#..##..|..#|.||#...|..##...##|....##||.##....
#|##..#|.#..|##...|..#.##.|##.....###.|..#.|..#.|.
|.##..|#...|.|.||.......#..#||.....#|..#||##..#|..
..|.#.#.....##.|#|...#........##......#...#...||..
|.#....###|..|##.#...#|....|..#.....#.##.|..|...||
.....#..#.....|.##......#......|..|...##|.|.#..#||
...##.#.......#|.#..||.#|..#|...#...|||.#.......|#
#|..#|....|||...|..#|....#......#..#...|#.......||
...#|##|..........|..###||..#|...|.##.|.#.#...#...
#|##|.#|#...|..#......||..#.|#|..#..|..#|..#......
#||#.#.....|...|..|##|..#|...##.||..#|.|#||.|..|..
#..#..|.|.||...#|.|.|..|..|..|....#.#||.#.....|#.#
#.|.#..##...|..#.|..#..#..#.#||.#.............#...
..|##|.#|.|......|#...|#.#.....|#|#.#.|...|#......
.|.|.|...#..##..#|###..|#....#..#.#..|||.###|##...
|#...|......|...##..|.|#...#..|.#.........#..##.#.
.|...##||#.....#..#..|..#..#.|#.|.||.##.|....|..#|
|#..|..|.#..||...#...#|..##|||##..|.##||#.#.|....|
.......#......|.....||.#..|#.#.#|#.##....|...|.#..
.....#..|...|..##.....|...#...|.|||.##..|.#||.##|.
..#||...|#.#|#|....#..|||.|##..#|.|.........|....#
..#...|.#...|#..#........#...###..##..##||...|..#.
..|.||.#.....|#..|.##...#.|...|#...#||..####..#.|.
.|.....#....||.#...#.......#|........#...#|#|...|#")

(def input-small ".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.")

(defn forest-to-outcome
  [forest]
  (apply * (map #(count (second %)) (filter #(or (= (first %) "|") (= (first %) "#"))
                                            (group-by str (vec forest))))))

(count (second (first (filter #(= "l" (first %))
                              (group-by str (map #(nth (str/join (str/split-lines input-small)) %) [1 2 3]))))))

(defn how-many
  [type
   forest]
  (count (second (first (filter #(= type (first %)) (group-by str forest))))))

(how-many "#" (map #(nth (str/join (str/split-lines input-small)) %) [1 2 3]))

(nth "abc" 2)

(first (str/join (str/split-lines input)))

(defn adjacent-acres
  [acre-number
   max-x
   max-y]
  (let [x (if (<= acre-number max-x) acre-number (if (= 0 (mod acre-number max-x)) max-x (mod acre-number max-x)))
        y (if (<= acre-number max-x) 1 (+ (if (= 0 (mod acre-number max-x)) 0 1) (int (/ acre-number max-x))))
        possible [[(- (- acre-number max-x) 1) (- x 1) (- y 1)]
                  [(- acre-number max-x) x (- y 1)]
                  [(+ (- acre-number max-x) 1) (+ x 1) (- y 1)]
                  [(- acre-number 1) (- x 1) y] [(+ acre-number 1) (+ x 1) y]
                  [(- (+ acre-number max-x) 1) (- x 1) (+ y 1)]
                  [(+ acre-number max-x) x (+ y 1)]
                  [(+ (+ acre-number max-x) 1) (+ x 1) (+ y 1)]]]
    (map first (filter #(and (and (> (second %) 0) (<= (second %) max-x))
                             (and (> (last %) 0) (<= (last %) max-y))) possible))))

(adjacent-acres 1 10 10)

(int (/ 18 6))

(mod 24 6)

[1 2 3 4]

(defn change-acre
  [acre
   acres-around]
  (if (= \. acre)
    (if (>= (how-many "|" acres-around) 3)
      \|
      \.)
    (if (= \| acre)
      (if (>= (how-many "#" acres-around) 3)
        \#
        \|)
      (if (= \# acre)
        (if (and (>= (how-many "#" acres-around) 1) (>= (how-many "|" acres-around) 1))
          \#
          \.)))))

(>= (how-many "|" "#|##|||") 3)

(change-acre \# ".#.|.")

(defn fast-forward-in-time
  [from-minute
   cycle
   to-minute
   history]
  (let [cycle-starts (- from-minute cycle)
        minutes-after-cycle-starts (mod (- to-minute cycle-starts) cycle)]
    (forest-to-outcome (:forest (first (filter #(= (+ cycle-starts minutes-after-cycle-starts) (:minute %)) history))))))

(defn grow
  [input
   minutes]
  (let [lines (str/split-lines input)
        forest (str/join lines)
        max-x (count (first lines))
        max-y (count lines)
        total-acres (* max-x max-y)]
    (loop [forest forest
           acres-left forest
           history []
           new-forest []
           time-limit 0]
      (if (= time-limit minutes)
        (forest-to-outcome (str/join forest))
        (if (empty? acres-left)
          ;history repeats for the first time
          (let [forest (str/join new-forest)
                repeats (filter #(= forest (:forest %)) history)
                current-minute (inc time-limit)]
            (if (= 1 (count repeats))
              (fast-forward-in-time current-minute (- current-minute (:minute (first repeats))) minutes history)
              (recur forest forest (conj history {:forest forest :minute (inc time-limit)}) [] (inc time-limit))))
          (let [acre-number (+ 1 (- total-acres (count acres-left)))
                acre (first acres-left)
                adjacent-numbers (adjacent-acres acre-number max-x max-y)
                ;nth starts from 0
                acres-around (map #(nth forest (- % 1)) adjacent-numbers)]
            (recur forest (rest acres-left) history (conj new-forest (change-acre acre acres-around)) time-limit)))))))

(conj (conj [] "a") "N")

(map #(nth "asdada" %) [1 2 4])

(= 1147 (grow input-small 10))
;solved P1
(= 536370 (grow input 10))

;190512 solved P2
(grow input 1000)
;first repetition at 597 minutes (same forest occurred at 569 minutes), so cycle is 28 minutes
;569 + 11 = 580 is the forest that occurs at the 1000000000 minute
(mod (- 1000000000 569) 28) ;11

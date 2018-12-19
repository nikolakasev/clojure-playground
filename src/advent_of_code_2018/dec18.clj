(ns advent-of-code-2018.dec18
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

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

(defn grow
  [input
   seconds]
  (let [lines (str/split-lines input)
        forest (str/join lines)
        max-x (count (first lines))
        max-y (count lines)
        total-acres (* max-x max-y)]
    (loop [forest forest
           acres-left forest
           new-forest []
           time-limit 0]
      (if (= time-limit seconds)
        (forest-to-outcome (str/join forest))
        (if (empty? acres-left)
          (recur (str/join new-forest) (str/join new-forest) [] (inc time-limit))
          (let [acre-number (+ 1 (- total-acres (count acres-left)))
                acre (first acres-left)
                adjacent-numbers (adjacent-acres acre-number max-x max-y)
                ;nth starts from 0
                acres-around (map #(nth forest (- % 1)) adjacent-numbers)]
            (recur forest (rest acres-left) (conj new-forest (change-acre acre acres-around)) time-limit)))))))

(conj (conj [] "a") "N")

(= 1147 (grow input 10))

(grow input 100) ;149996
(grow input 1000) ;190512
(grow input 10000)

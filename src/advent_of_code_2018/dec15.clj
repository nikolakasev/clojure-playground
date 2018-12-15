(ns advent-of-code-2018.dec15
  (:require [clojure.string :as str])
  (:require [loom.graph :as g])
  (:require [loom.io :as io])
  (:require [loom.alg :as a]))

(def g (g/graph [1 2] [2 3] {3 [4] 5 [6 7]} 7 8 9))

(def g2 (g/graph [1 2] [2 3] [3 4] [1 4]))

(io/view g2)

(g/successors g 6)

(a/bf-path g 1 3)

(io/view (g/add-nodes g "foobar" {:name "baz"} [1 2 3]))

(def wg (g/weighted-graph {:a {:b 10 :c 20} :c {:d 30} :e {:b 5 :d 5}}))

(io/view wg)

(a/bf-path wg :e :a)

(def board (g/weighted-graph {:1-1 {:2-1 3 :1-2 4} :2-1 {:1-1 2 :3-1 3 :2-2 4} :3-1 {:2-1 2 :4-1 3 :3-2 4} :4-1 {:3-1 2 :5-1 3} :5-1 {:4-1 2 :5-2 4} :5-2 {:5-1 1 :5-3 4} :5-3 {:5-2 1} :1-2 {:2-2 3 :1-3 4} :2-2 {:2-1 1 :1-2 2 :3-2 3 :2-3 4} :3-2 {:3-1 1 :2-2 2 :3-3 4} :1-3 {:1-2 1 :2-3 3} :2-3 {:2-2 1 :1-3 2 :3-3 3} :3-3 {:3-2 1 :2-3 2}}))

(def board-wdg (g/weighted-digraph {:1-1 {:2-1 3 :1-2 4} :2-1 {:1-1 2 :3-1 3 :2-2 4} :3-1 {:2-1 2 :4-1 3 :3-2 4} :4-1 {:3-1 2 :5-1 3} :5-1 {:4-1 2 :5-2 4} :5-2 {:5-1 1 :5-3 4} :5-3 {:5-2 1} :1-2 {:2-2 3 :1-3 4} :2-2 {:2-1 1 :1-2 2 :3-2 3 :2-3 4} :3-2 {:3-1 1 :2-2 2 :3-3 4} :1-3 {:1-2 1 :2-3 3} :2-3 {:2-2 1 :1-3 2 :3-3 3} :3-3 {:3-2 1 :2-3 2}}))

(io/view board)
(io/view board-wdg)

(g/successors board-wdg :2-2)

(a/dijkstra-path board-wdg :3-1 :1-3)

(a/bf-traverse board-wdg :5-3)

(defn not-an-elf
  [neighbor predecessor depth]
  (not (= neighbor :4-1)))

(a/bf-traverse board-wdg :5-3 :when not-an-elf)

(defn battle
  [input]
  0)

(= 27730 (battle "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"))

(= 36334 (battle "#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######"))

(= 39514 (battle "#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######"))

(= 27755 (battle "#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######"))

(= 28944 (battle "#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######"))

(= 18740 (battle "#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########"))

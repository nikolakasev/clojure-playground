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

(io/view (g/weighted-digraph {:0-0 {:1-2 3, :1-3 4}}))

(g/successors board-wdg :2-2)

(a/dijkstra-path board-wdg :3-1 :1-3)

(a/bf-traverse board-wdg :5-3)

(defn not-an-elf
  [neighbor predecessor depth]
  (not (= neighbor :4-1)))

(a/bf-traverse board-wdg :5-3 :when not-an-elf)

(def input "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")

(def lines (map vec (str/split-lines input)))

(nth (nth lines 3) 1)

(defn neighbours
  "Returns a set of neighbouring locations and their corresponding weights."
  [lines
   [x y]]
  (let [character (nth (nth lines y) x)]
    (if (= \# character)
      nil
      (let [top [(nth (nth lines (- y 1)) x) (keyword (str x "-" (- y 1))) 1]
            bottom [(nth (nth lines (+ y 1)) x) (keyword (str x "-" (+ y 1))) 4]
            left [(nth (nth lines y) (- x 1)) (keyword (str (- x 1) "-" y)) 2]
            right [(nth (nth lines y) (+ x 1)) (keyword (str (+ x 1) "-" y)) 3]]
        (apply array-map (mapcat (fn [[_ location weight]] [location weight]) (filter #(not (= \# (first %))) [top bottom left right])))))))

(neighbours lines [1 4])

(defn input-to-board
  [input]
  (let [lines (map vec (str/split-lines input))
        max-x (count (first lines))
        max-y (count lines)]
    ;don't bother with the outside walls
    (for [y (range 1 (- max-y 1))
          x (range 1 (- max-x 1))]
      (if (not (= \# (nth (nth lines y) x)))
        [(keyword (str x "-" y)) (neighbours lines [x y])]))))

(defn input-to-actors
  [input
   initial-health]
  (let [lines (map vec (str/split-lines input))
        max-x (count (first lines))
        max-y (count lines)
        ;don't bother with the outside walls
        actors (for [y (range 1 (- max-y 1))
                     x (range 1 (- max-x 1))]
                 (let [character (nth (nth lines y) x)]
                   (if (and (not (= \# character)) (not (= \. character)))
                     {:type (str character) :location [x y] :hp initial-health})))]
    (filter (comp not nil?) actors)))

(def actors (input-to-actors input 300))

(:E (first actors))

(:1 (first (concat [{:1 2}] [{:3 4}])))

;builds the graph
(def b (g/weighted-digraph (apply hash-map (mapcat identity (input-to-board input)))))

(count (g/nodes b))

(let [x 1]
  (str (+ x 1) "-" x))

(= \# \.)

(defn enemies-left
  [actors]
  (= 2 (count (group-by :type actors))))

(enemies-left actors)

(defn total-health
  [actors]
  (apply + (map :hp actors)))

(total-health actors)

(def sort-by-reading-order
  (partial sort-by #(+ (first (:location %))
                       (* 10 (second (:location %))))))

(sort-by-reading-order actors)

(group-by first actors)

(defn battle
  [input]
  (let [board (g/weighted-digraph (apply hash-map (mapcat identity (input-to-board input))))
        actors (sort-by-reading-order (input-to-actors input))]
    (loop [round 0
           ;changes only when an actor dies
           all-actors actors
           ;for each actor apply it's logic
           actors-left actors]
      (if (enemies-left all-actors)
        ;continue round
        (let [actor (first actors-left)
              adjacent-enemies []
              enemy-coordinates []
              enemies-in-range []]
          (if (not (empty? adjacent-enemies))))
            ;attack the enemy with the lowest health
            ;move

        ;battle ends
        (* (dec round) (total-health all-actors))))))

(defn move-actor
  "Moves an actor from one coordinate to another."
  [[x y]
   [new-x new-y]
   all-actors])

(move-actor)

(defn attack-actor
  [location
   all-actors
   f]
  (map #(if (= location (:location %)) (update % :hp f) %) all-actors))

(= [1 2] [1 4])

(:location (first actors))

(attack-actor [3 4] actors (fn [hp] (- hp 3)))

({:type "G", :location [2 1], :hp 300} {:type "E", :location [4 2], :hp 300} {:type "G", :location [5 2], :hp 300} {:type "G", :location [5 3], :hp 300} {:type "G", :location [3 4], :hp 300} {:type "E", :location [5 4], :hp 300})

(defn remove-dead-actors
  [actors]
  (filter #(> (:hp %) 0) actors))

(not (empty? [1 2]))

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

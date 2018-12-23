(ns advent-of-code-2018.dec15
  (:require [clojure.string :as str])
  (:require [loom.graph :as g])
  (:require [loom.io :as io])
  (:require [loom.alg :as a])
  (:require [clojure.set :as set]))

(defmacro dlet
  "let with inspected bindings"
  [bindings & body]
  `(let [~@(mapcat (fn [[n v]]
                     (if (or (vector? n) (map? n))
                       [n v]
                       [n v '_ `(println (name '~n) ":" ~v)]))
                   (partition 2 bindings))]
     ~@body))

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

(defn node-to-location
  [node]
  (let [s (name node)
        regex #"(\d*)-(\d*)"
        matches (re-matches regex s)]
    [(read-string (second matches)) (read-string (last matches))]))

(node-to-location :1-2)

(name :1-2)

(defn location-to-node
  [[x y]]
  (keyword (str x "-" y)))

(defn board-size
  [input]
  (let [lines (map vec (str/split-lines input))
        max-x (count (first lines))
        max-y (count lines)]
    [max-x max-y]))

(defn input-to-board
  [input]
  (let [lines (map vec (str/split-lines input))
        [max-x max-y] (board-size input)]
    ;don't bother with the outside walls
    (for [y (range 1 (- max-y 1))
          x (range 1 (- max-x 1))]
      (if (not (= \# (nth (nth lines y) x)))
        [(location-to-node [x y]) (neighbours lines [x y])]))))

(input-to-board input)

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

(def actors (input-to-actors input 200))

(first actors)

(:E (first actors))

(:1 (first (concat [{:1 2}] [{:3 4}])))

;builds the graph
(def b (g/weighted-digraph (apply hash-map (mapcat identity (input-to-board input)))))

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

;TODO re-factor with locations-by-reading-order?
(def actors-by-reading-order
  (partial sort-by #(+ (first (:location %))
                       (* 10 (second (:location %))))))

(actors-by-reading-order actors)

(def locations-by-reading-order
  (partial sort-by #(+ (first %)
                       (* 10 (second %)))))

(defn reading-order-sort-by
  "Sort a `coll` of maps using a `sort-function`, the return one location by reading order."
  [sort-function
   location-function
   coll]
  (let [lowest-hp-first (sort-by sort-function coll)
        possible (reduce (fn [coll item]
                           (if (or (empty? coll) (= (sort-function (first coll)) (sort-function item)))
                             (concat coll [item])
                             coll))
                         []
                         lowest-hp-first)]
    (first (locations-by-reading-order (map location-function possible)))))

(reading-order-sort-by :hp :location [{:type "G", :hp 1, :location [2 1]}
                                      {:type "G", :hp 156, :location [2 1]}
                                      {:type "G", :hp 140, :location [3 1]}
                                      {:type "G", :hp 200, :location [3 1]}])

(locations-by-reading-order [[1 2] [1 1] [3 4]])

(group-by first actors)

(defn move-actor
  "Moves an actor from a `location` to `new-location`."
  [location
   new-location
   all-actors]
  (map #(if (= location (:location %)) (update % :location (fn [loc] new-location)) %) all-actors))

(move-actor [3 4] [10 10] actors)

(identity [1 2])

(update (first actors) :hp (fn [a] (+ 10 a)))

(defn attack-actor
  "Applies attack function `f` on an actor which is located on `location`."
  [location
   all-actors
   f]
  ;"select for update" pattern on a list of maps
  (map #(if (= location (:location %)) (update % :hp f) %) all-actors))

(= [1 2] [1 4])

(:location (first actors))

(sort-by :hp (attack-actor [3 2] actors #(- % 3)))

(filter #(<= (:hp %) 0) (attack-actor [3 2] actors #(- % 3)))

(def locs (set (map :location (take 2 actors))))

(defn remove-dead-actors
  [dead
   actors]
  (let [locations (set (map :location dead))]
    (filter #(not (contains? locations (:location %))) actors)))

(remove-dead-actors (take 2 actors) actors)

(not (empty? [1 2]))

(defn enemies-of
  [actor-type
   all-actors]
  (filter #(not (= actor-type (:type %))) all-actors))

(defn allies-of
  [actor
   all-actors]
  ;same type, but not the same actor
  (filter #(and (= (:type actor) (:type %)) (not (= (:location actor) (:location %)))) all-actors))

(defn adjacent-enemies
  [actor-location
   actor-type
   all-actors
   board]
  (let [adjacent-nodes (set (g/successors board (location-to-node actor-location)))
        enemy-nodes (set (map (comp location-to-node :location) (enemies-of actor-type all-actors)))]
    ;a list of enemy actors
    (mapcat (fn [node] (filter #(= (node-to-location node) (:location %)) all-actors))
            (set/intersection adjacent-nodes enemy-nodes))))

(empty? (adjacent-enemies [3 2] "G" actors b))

(empty? (adjacent-enemies [3 1] "G" actors b))

(first actors)

(defn not-an-actor
  [actors neighbor predecessor depth]
  (not (contains? actors neighbor)))

(node-to-location :1-2)

(def input "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")

(defn draw-board
  [[max-x
    max-y]
   board
   actors]
  (for [y (range 1 (- max-y 1))
        x (range 1 (- max-x 1))]
    (let [found (filter #(= [x y] (:location %)) actors)]
      (if (contains? (g/nodes board) (location-to-node [x y]))
        (if (= 1 (count found)) (:type (first found)) ".")
        "#"))))

(defn write-area-to-file
  [area
   filename]
  (with-open [w (clojure.java.io/writer filename)]
    (doseq [line area]
      (.write w line)
      (.newLine w))))

(defn board-to-file
  [[max-x max-y]
   board
   actors
   file-name]
  (let [lines (partition (- max-x 2) (draw-board [max-x max-y] board actors))
        lines-as-string (map str/join lines)]
    (write-area-to-file lines-as-string file-name)))

(defn steps-to-location
  [from-location
   actors-in-the-way
   board
   to-location]
  (let [path (a/bf-path board
                        (location-to-node from-location) (location-to-node to-location)
                        :when (partial not-an-actor actors-in-the-way))]
    ;the path includes the first step, so discard it
    {:location to-location :steps-to-reach (dec (count path))}))

(steps-to-location [3 4] [] b [5 5])

(map (comp (partial steps-to-location [3 4] [] b) node-to-location) [:4-1 :3-2 :5-5])

(reading-order-sort-by :steps-to-reach :location
                       (map (partial steps-to-location [3 4] [] b) [[4 1] [3 2] [5 5]]))

(first actors)

(io/view b)

(a/dijkstra-path b :3-3 :1-3)
;(node-to-location (second (a/dijkstra-path (apply g/remove-nodes b [:2-2]) :3-3 :2-2)))

(defn battle
  [input]
  (let [board (g/weighted-digraph (apply hash-map (mapcat identity (input-to-board input))))
        actors (actors-by-reading-order (input-to-actors input 200))
        attack-function #(- % 3)]
    (board-to-file (board-size input) board actors "src/advent_of_code_2018/board-initial.txt")
    (loop [round 1
           ;changes only when an actor dies
           all-actors actors
           ;for each actor apply it's logic
           actors-left actors]
      (board-to-file (board-size input) board all-actors (str "src/advent_of_code_2018/board-" round ".txt"))
      (if (enemies-left all-actors)
        (if (empty? actors-left)
          ;round ends
          (recur (inc round)
                 (actors-by-reading-order all-actors)
                 (actors-by-reading-order all-actors))
          ;round continues
          (let [actor (first actors-left)
                able-to-attack (adjacent-enemies (:location actor) (:type actor) all-actors board)
                ;TODO refactor next two and "inject" enemies-of or allies-of?
                enemy-nodes (set (map (comp location-to-node :location) (enemies-of (:type actor) all-actors)))
                allied-nodes (set (map (comp location-to-node :location) (allies-of actor all-actors)))
                other-actor-nodes (set/union enemy-nodes allied-nodes)
                reachable-locations (a/bf-traverse board
                                                   (location-to-node (:location actor))
                                                   :when (partial not-an-actor allied-nodes))
                reachable-enemies (set/intersection (set reachable-locations)
                                                    enemy-nodes)]
            (if (not (empty? able-to-attack))
              ;attack! the enemy with the lowest health and clean up the mess
              (let [enemy-at-location (reading-order-sort-by :hp :location able-to-attack)
                    actors-after-attack (attack-actor enemy-at-location all-actors attack-function)
                    dead-actors (filter #(<= (:hp %) 0) actors-after-attack)]
                (recur round
                       (remove-dead-actors dead-actors actors-after-attack)
                       (rest (remove-dead-actors dead-actors actors-left))))
              (if (not (empty? reachable-enemies))
                ;move if there are reachable enemies
                (let [in-range (set/difference (set (mapcat #(g/successors board %) reachable-enemies))
                                               other-actor-nodes)
                      target (reading-order-sort-by :steps-to-reach :location
                                                    ;this is beauty :) first turn a node to location, then calculate the steps
                                                    (map (comp (partial steps-to-location (:location actor) other-actor-nodes board)
                                                               node-to-location)
                                                         in-range))
                      board-without-allies (apply g/remove-nodes board allied-nodes)
                      path-to-target (a/dijkstra-path board-without-allies
                                                      (location-to-node (:location actor)) (location-to-node target))
                      move-to (node-to-location (second path-to-target))
                      ;will there be adjacent enemies after the move?
                      will-be-able-to-attack (adjacent-enemies move-to (:type actor) all-actors board)]
                  (if (not (empty? will-be-able-to-attack))
                    ;leave the actor, so it can attack right after moving
                    (recur round
                           (move-actor (:location actor) move-to all-actors)
                           (move-actor (:location actor) move-to actors-left))
                    (recur round
                           (move-actor (:location actor) move-to all-actors)
                           (rest actors-left))))
                ;nothing to do because there are no adjacent or reachable enemies
                (recur round
                       all-actors
                       (rest actors-left))))))
        ;battle ends
        ;(board-to-file (board-size input) board all-actors "src/advent_of_code_2018/out.txt")))))
        (* (if (empty? actors-left) round (dec round)) (total-health all-actors))))))

(battle "#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########")

(= 27730 (battle "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")) ;incorrect position, G3 moves to the wrong target

(adjacent-enemies [4 1] (:type (first actors)) actors b)

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

(= 1 1)

;200410 isn't it, 203273 neither
(battle (slurp "src/advent_of_code_2018/input-dec15.txt"))

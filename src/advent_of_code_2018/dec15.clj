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

(defn input-to-board
  [input]
  (let [lines (map vec (str/split-lines input))
        max-x (count (first lines))
        max-y (count lines)]
    ;don't bother with the outside walls
    (for [y (range 1 (- max-y 1))
          x (range 1 (- max-x 1))]
      (if (not (= \# (nth (nth lines y) x)))
        [(location-to-node [x y]) (neighbours lines [x y])]))))

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
  [actor
   all-actors]
  (filter #(not (= (:type actor) (:type %))) all-actors))

(defn allies-of
  [actor
   all-actors]
  ;same type, but not the same actor
  (filter #(and (= (:type actor) (:type %)) (not (= (:location actor) (:location %)))) all-actors))

(defn adjacent-enemies
  [actor
   all-actors
   board]
  (let [adjacent-nodes (set (g/successors board (location-to-node (:location actor))))
        enemy-nodes (set (map (comp location-to-node :location) (enemies-of actor all-actors)))]
    ;a list of enemy actors
    (mapcat (fn [node] (filter #(= (node-to-location node) (:location %)) all-actors))
            (set/intersection adjacent-nodes enemy-nodes))))

(adjacent-enemies (second actors) actors b)

(defn not-an-ally
  [allies neighbor predecessor depth]
  (not (contains? allies neighbor)))

(node-to-location :1-2)

(def input "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")

(def board (g/weighted-digraph (apply hash-map (mapcat identity (input-to-board input)))))

(def all-actors (input-to-actors input 200))
(def actor (first all-actors))

(def enemy-nodes (set (map (comp location-to-node :location) (enemies-of actor all-actors))))
(def allied-nodes (set (map (comp location-to-node :location) (allies-of actor all-actors))))
(def reachable-enemies (set/intersection (set (a/bf-traverse board (location-to-node (:location actor)) :when (partial not-an-ally allied-nodes)))
                                         enemy-nodes))

(def in-range (set/difference (set (mapcat #(g/successors board %) reachable-enemies))
                              (set (map (comp location-to-node :location) all-actors))))
(def target (first (locations-by-reading-order (map node-to-location in-range))))
(def path-to-target (a/dijkstra-path board (location-to-node (:location actor)) (location-to-node target)))
(def path-bf (a/bf-path board (location-to-node (:location actor)) (location-to-node target) :when (partial not-an-ally allied-nodes)))

(defn battle
  [input]
  (let [board (g/weighted-digraph (apply hash-map (mapcat identity (input-to-board input))))
        actors (actors-by-reading-order (input-to-actors input 200))
        attack-function #(- % 3)]
    (loop [round 0
           ;changes only when an actor dies
           all-actors actors
           ;for each actor apply it's logic
           actors-left actors]
      (if (enemies-left all-actors)
        (if (empty? actors-left)
          ;round ends
          (recur (inc round)
                 (actors-by-reading-order all-actors)
                 (actors-by-reading-order all-actors))
          ;round continues
          (let [actor (first actors-left)
                ;to be able to attack
                adjacent-enemies (adjacent-enemies actor all-actors board)
                ;TODO refactor next two and "inject" enemies-of or allies-of?
                enemy-nodes (set (map (comp location-to-node :location) (enemies-of actor all-actors)))
                allied-nodes (set (map (comp location-to-node :location) (allies-of actor all-actors)))
                reachable-enemies (set/intersection (set (a/bf-traverse board (location-to-node (:location actor)) :when (partial not-an-ally allied-nodes)))
                                                    enemy-nodes)]
            (if (not (empty? adjacent-enemies))
              ;attack! the enemy with the lowest health and clean up the mess
              (let [enemy-to-attack (first (sort-by :hp adjacent-enemies))
                    actors-after-attack (attack-actor (:location enemy-to-attack) all-actors attack-function)
                    dead-actors (filter #(<= (:hp %) 0) actors-after-attack)]
                (recur round
                       (remove-dead-actors dead-actors actors-after-attack)
                       (rest (remove-dead-actors dead-actors actors-left))))
              (if (not (empty? reachable-enemies))
                ;move if there are reachable enemies
                (let [in-range (set/difference (set (mapcat #(g/successors board %) reachable-enemies))
                                               (set (map (comp location-to-node :location) all-actors)))
                      target (first (locations-by-reading-order (map node-to-location in-range)))
                      path-to-target (a/bf-path board (location-to-node (:location actor)) (location-to-node target) :when (partial not-an-ally allied-nodes))
                      move-to (node-to-location (second path-to-target))]
                  (recur round
                         (move-actor (:location actor) move-to all-actors)
                         (rest actors-left)))
                ;nothing to do because there are no adjacent or reachable enemies
                (recur round
                       all-actors
                       (rest actors-left))))))
        ;battle ends
        all-actors))));(* round (total-health all-actors))))))

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

board (g/weighted-digraph (apply hash-map (mapcat identity (input-to-board input))))
actors (actors-by-reading-order (input-to-actors input 200))

(g/nodes board)

(def b-string (draw-board [7 7] board actors))
(partition (- 7 2) b-string)

(defn write-area-to-file
  [area
   filename]
  (with-open [w (clojure.java.io/writer filename)]
    (doseq [line area]
      (.write w line)
      (.newLine w))))

(write-area-to-file ["abc" "cde"] "src/advent_of_code_2018/out.txt")

(defn board-to-file
  [[max-x max-y]
   board
   actors
   file-name]
  (let [lines (partition (- max-x 2) (draw-board [max-x max-y] board actors))]
    lines))

(board-to-file [7 7] board actors "src/advent_of_code_2018/out.txt")

(io/view board)

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
#######"))

(= 36334 (battle "#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######"))

(= 1 1)

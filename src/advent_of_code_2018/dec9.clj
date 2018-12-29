(ns advent-of-code-2018.dec9
  (:require [clojure.data.finger-tree :as tree]))

(defn marble-in-tree
  [marble
   tree
   current-marble-position]
  (if (= 1 marble)
    [(conj tree marble) 1]
    ;current marble is second to last
    (if (= (inc current-marble-position) (dec (count tree)))
      [(conj tree marble) (+ 2 current-marble-position)]
      ;current marble is last
      (if (= (inc current-marble-position) (count tree))
        (let [[left current right] (tree/ft-split-at tree 1)]
          [(tree/ft-concat (conj left marble) (tree/conjl right current)) 1])
        (let [[left current right] (tree/ft-split-at tree (inc current-marble-position))]
          [(tree/ft-concat (conj left current) (tree/conjl right marble)) (+ 2 current-marble-position)])))))

(def tree (apply tree/counted-double-list [0 8 4 2 5 1 6 3 7]))

(marble-in-tree 9 tree 1)

(:left (tree/ft-split-at tree 1))

(conj tree 8)

(defn marble-from-tree
  [marble-position
   tree]
  (let [[left marble right] (tree/ft-split-at tree marble-position)]
    [marble (tree/ft-concat left right)]))

(marble-from-tree 1 tree)

(defn player-scores
  [player
   points
   players-map]
  (if (nil? (get players-map player))
    (conj players-map [player points])
    (update players-map player #(+ % points))))

(defn put-marble
  [[marble-ring current-position player-map]
   [marble player]]
  ;(rem 0 23) returns 0
  (if (and (= 0 (rem marble 23)) (not (= 0 marble)))
    (let [seven-marbles-back (- current-position 7)
          marble-to-take (if (>= seven-marbles-back 0)
                           seven-marbles-back
                           (- (count marble-ring) (- 7 current-position)))
          [score ring] (marble-from-tree marble-to-take marble-ring)]
      [ring marble-to-take (player-scores player (+ marble score) player-map)])
    (let [[ring new-position] (marble-in-tree marble marble-ring current-position)]
      [ring new-position player-map])))

(defn marble-play
  [players
   last-marble]
  (let [[_ _ player-map] (reduce put-marble
                          ;game starts with marble 0 in play, position of 0 and no scores
                                 [(apply tree/counted-double-list [0]) 0 {}]
                          ;to be able to extract in put-marble
                                 (map vec
                               ;zip the players and marbles, so each player is assigned a marble
                                      (partition 2 (interleave (range 1 (inc last-marble))
                                                               (cycle (range 1 (inc players)))))))]
    (second (first (reverse (sort-by last player-map))))))

(second (first (reverse (sort-by last (conj {1 100, 2 300} [3 200])))))

(player-scores 2 50 {1 100, 2 300})

(get {1 100, 2 300} 2)

(conj {1 100, 2 300} [3 400])

;NullPointerException
;(update {1 100, 2 300} 3 inc)

(update {} :a (fnil inc 0))

(map vec (partition 2 (interleave (range 1 9) (cycle (range 1 4)))))

;10 players; last marble is worth 1618 points: high score is 8317
(= 8317 (marble-play 10 1618))
;13 players; last marble is worth 7999 points: high score is 146373
(= 146373 (marble-play 13 7999))
;17 players; last marble is worth 1104 points: high score is 2764
(= 2764 (marble-play 17 1104))
;21 players; last marble is worth 6111 points: high score is 54718
(= 54718 (marble-play 21 6111))
;30 players; last marble is worth 5807 points: high score is 37305
(= 37305 (marble-play 30 5807))

;416 players; last marble is worth 71975 points
;439341 solved P1, runs for about 4 seconds
(time (marble-play 416 71975))
;Elapsed time: 605268.585448 msecs, solves P2
(time (marble-play 416 (* 100 71975)))

(def dl (tree/double-list 4 5 6 7))

(first dl)
(rest dl)
(pop dl)
(peek dl)

(def cdl
  (apply tree/counted-double-list '[a b c d e f g h i j k l m]))

(apply tree/counted-double-list [0])

(nth cdl 5)

(assoc cdl 5 'XX)

(def parts
  (let [[left _ right] (tree/ft-split-at cdl 5)]
    {:left left, :right right}))

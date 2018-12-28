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

(tree/ft-concat 0 (rest tree))

(defn put-marble
  [coll
   [marble player]]
  ;(rem 0 23) returns 0
  (if (and (= 0 (rem marble 23)) (not (= 0 marble)))
    (inc coll)
    coll))

(defn marble-play
  [players
   last-marble]
  (reduce put-marble
          ;game starts with marble 0 in play
          (apply tree/counted-double-list [0])
          ;to be able to extract in put-marble
          (map vec
               ;zip the players and marbles, so each player is assigned a marble
               (partition 2 (interleave (range 1 (inc last-marble))
                                        (cycle (range 1 (inc players))))))))

(get {1 100, 2 300} 2)

(marble-play 1 20)

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
(marble-play 416 71975)

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

(identity parts)

(rem 45 23)

(range 0 3)

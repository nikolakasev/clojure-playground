(ns advent-of-code-2018.dec5
  (:require [clojure.string :as str])
  (:require [clojure.math.numeric-tower :as math]))

(def input-small "dabAcCaCBAcCcaDA")

(defn react?
  [unit-one
   unit-two]
  (= 32 (math/abs (- (int unit-one) (int unit-two)))))

(react? \a \z)

(defn reduce-polymer
  [polymer]
  (reduce (fn [so-far unit]
            (if (or (empty? so-far) (not (react? (peek so-far) unit)))
              (conj so-far unit)
              (pop so-far)))
          []
          polymer))

(= 10 (count (reduce-polymer input-small)))

(reduce-polymer "xabcCdDBA")

(def input (str/trim-newline (slurp "src/advent_of_code_2018/input-dec5.txt")))

;solves P1
(count (reduce-polymer input))

(mod 29 5)

(- (int \b) (int \B))

(def large-vec (vec (range 0 10000)))

(conj (vec "abcds") \c)

(int \z)

(defn reduce-polymer-tweaked
  [polymer
   disregard-set]
  (reduce (fn [so-far unit]
            (if (contains? disregard-set unit)
              so-far
              (if (or (empty? so-far) (not (react? (peek so-far) unit)))
                (conj so-far unit)
                (pop so-far))))
          []
          polymer))

(reduce-polymer-tweaked "dabAcCaCBAcCcaDA" (set [\c \C]))

(range (int \a) (+ 1 (int \z)))

(- (int \a) 32) (int \A) (- 97 65)

(first (sort (map #(count (reduce-polymer-tweaked input-small (set [% (first (map char (str/upper-case %)))])))
                  (map char (range (int \a) (+ 1 (int \z)))))))

(set [\d (map char (str/upper-case \d))]) (map char "D")

;4944 solves P2
(first (sort (map #(count (reduce-polymer-tweaked input (set [% (first (map char (str/upper-case %)))])))
                  (map char (range (int \a) (+ 1 (int \z)))))))

(map char (range (int \a) (+ 1 (int \z))))

(-> {:year 2018}
    (update :year inc)
    (with-meta {:happy true}))

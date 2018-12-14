(ns advent-of-code-2018.dec12
  (:require [clojure.string :as str]))

(def input-small
  "#..#.#..##......###...###")

(def instruction-regex
  #"([\.#]*)\s=>\s([\.#])")

(def instructions
  (map (fn [[_ instruction plant-or-not]] [instruction plant-or-not]) (map (partial re-matches instruction-regex) (str/split-lines "...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #"))))

(re-matches #"(\.\.##\.\.)" input-small)

(re-find #"\.\.##\.\." input-small)

(str/index-of "#..#" input-small 1)

(str/index-of input-small "..#.#..##")

(re-matches instruction-regex "...## => #")

(let [m (re-matcher #"a" "bbbabbb")] (.find m) (.start m))

(let [m (re-matcher #"#." input-small)] (.find m 6) (.start m))

(defn string-to-generation
  [input
   begins-at-pot]
  (loop [characters (vec input)
         so-far #{}
         position begins-at-pot]
    (if (empty? characters)
      so-far
      (if (= \# (first characters))
        (recur (rest characters) (into so-far [position]) (inc position))
        (recur (rest characters) so-far (inc position))))))

(apply max (string-to-generation input-small 0))

(range 1 10)

(defn generation-to-string
  [generation
   begins-at-pot]
  (let [max-pot-on-the-right (apply max generation)]
    (str/join (map #(if (contains? generation %) "#" ".") (range begins-at-pot (+ 1 max-pot-on-the-right))))))

;input to structure and back works
(= (generation-to-string (string-to-generation input-small 0) 0) input-small)

;structure works for end result
(= 325 (apply + (string-to-generation "#....##....#####...#######....#.#..##." -2)))

(defn generations
  [generation
   instructions
   number-of-generations-wanted]
  [0 0])

;algo should work on the example
(= 325 (apply + (generations (string-to-generation "#..#.#..##......###...###." 0) instructions 20)))

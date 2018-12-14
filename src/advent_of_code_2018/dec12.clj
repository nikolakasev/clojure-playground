(ns advent-of-code-2018.dec12
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(def input-small
  "#..#.#..##......###...###")

(def instructions
  (str/split-lines "...## => #
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
####. => #"))

(re-matches #"(\.\.##\.\.)" input-small)

(re-find #"\.\.##\.\." input-small)

(str/index-of "#..#" input-small 1)

(str/index-of input-small "..#.#..##")

(re-matches #"([\.#]*)\s=>\s([\.#])" "...## => #")

(let [m (re-matcher #"a" "bbbabbb")] (.find m) (.start m))

(let [m (re-matcher #"#." input-small)] (.find m 6) (.start m))

(defn generation
  [input]
  (loop [characters (vec input)
         so-far #{}
         position 0]
    (if (empty? characters)
      so-far
      (if (= \# (first characters))
        (recur (rest characters) (into so-far [position]) (inc position))
        (recur (rest characters) so-far (inc position))))))

(generation input-small)

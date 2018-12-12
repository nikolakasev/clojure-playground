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

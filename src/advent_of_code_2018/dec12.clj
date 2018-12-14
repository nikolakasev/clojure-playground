(ns advent-of-code-2018.dec12
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(def input-small
  "#..#.#..##......###...###")

(def instruction-regex
  #"([\.#]*)\s=>\s([\.#])")

(def instructions
  (map first (map (fn [[_ instruction plant-or-not]] [instruction plant-or-not]) (map (partial re-matches instruction-regex) (str/split-lines "...## => #
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
####. => #")))))

(re-matches #"(\.\.##\.\.)" input-small)

(re-find #"\.\.##\.\." input-small)

(str/index-of "#..#" input-small 0)

(str/index-of input-small "#..#.#..##" 0)

(re-matches instruction-regex "...## => #")

(let [m (re-matcher #"f" "bbbabbb")] (.find m))

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

(defn apply-instruction
  [generation
   begins-at-pot
   instruction]
  (let [gen-string (generation-to-string generation begins-at-pot)]
    (loop [index 0
           so-far #{}]
      (let [found (str/index-of (str "...." gen-string "....") instruction index)]
        (if (nil? found)
          so-far
          (recur (inc found) (into so-far [(+ (+ begins-at-pot -4) (+ found 2))])))))))

(defn generations
  [generation
   instructions-for-plant
   instructions-for-empty
   number-of-generations-wanted]
  (loop [number 0
         begins-at-pot 0
         latest-generation generation]
    (if (= number number-of-generations-wanted)
      latest-generation
      (let [pots-with-plants (set (mapcat (partial apply-instruction latest-generation begins-at-pot) instructions-for-plant))
            pots-to-empty (set (mapcat (partial apply-instruction latest-generation begins-at-pot) instructions-for-empty))
            plants (set/difference pots-with-plants pots-to-empty)]
        (if (empty? plants)
          (recur (inc number) begins-at-pot plants)
          (recur (inc number) (apply min plants) plants))))))

(mapcat (partial apply-instruction (string-to-generation "#..#.#..##......###...###." 0) 0) instructions)

(def gen
  (string-to-generation "#..#.#..##......###...###" 0))

(str/index-of (str ".." (generation-to-string gen 0) "..") "..#..")

(apply-instruction gen 0 "..#..")

(generation-to-string (string-to-generation "#..#.#..##......###...###." 0) 0)

(set/difference #{0 3 4 5 6} #{})

(apply max #{})

(apply-instruction (string-to-generation input-small 0) 0 "..#..")

;algo should work on the example
(= 325 (apply + (generations (string-to-generation "#..#.#..##......###...###." 0) instructions #{} 20)))

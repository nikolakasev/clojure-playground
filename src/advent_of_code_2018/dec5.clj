(ns advent-of-code-2018.dec5
  (:require [clojure.string :as str]))

(def input-small "dabAcCaCBAcCcaDA")

(vec input-small)

(str/lower-case \B)

(defn drop-opposite-polarity
  [input]
  (loop [units-remaining []
         units-left input]
    (if (<= (count units-left) 1)
      (concat units-remaining units-left)
      (let [unit (first units-left)
            neighbour (second units-left)]
        (if (or (= (str unit) (str/lower-case neighbour)) (= (str unit) (str/upper-case neighbour)))
          (recur units-remaining (drop 2 units-left))
          (recur (conj units-remaining unit) (rest units-left)))))))

(defn reduce-polymer
  [input]
  (loop [polymer-so-far input
         polymer-length (count input)]
    (let [reduced-polymer (drop-opposite-polarity polymer-so-far)]
      (if (= polymer-length (count reduced-polymer))
        (str/join reduced-polymer)
        (do
          (println "reduced to " (count reduced-polymer))
          (recur reduced-polymer (count reduced-polymer)))))))

(= 10 (count (reduce-polymer "dabAcCaCBAcCcaDA")))

(def input (vec (slurp "src/advent_of_code_2018/input-dec5.txt")))

(count (reduce-polymer input))

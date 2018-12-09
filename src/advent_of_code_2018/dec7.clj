(ns advent-of-code-2018.dec7
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(set [:A :B :C])

(disj (set [:A :B :C]) :A)

(set/difference (set [:A :B :C]) (set [:A :B]))

(def regex #"\w+\s([A-Z])\smust\sbe\sfinished\sbefore\sstep\s([A-Z])\scan\sbegin.")

(def input-small
  (map (fn [[step & dependencies]] [step dependencies])
       (map (fn [step] (distinct (flatten (second step))))
            (group-by first (map (fn [matches] [(keyword (second matches)) (last matches)]) (map (partial re-matches regex) (str/split-lines "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")))))))

(def input
  (map (fn [[step & dependencies]] [step dependencies])
       (map (fn [step] (distinct (flatten (second step))))
            (group-by first (map (fn [matches] [(keyword (second matches)) (last matches)]) (map (partial re-matches regex) (str/split-lines (slurp "src/advent_of_code_2018/input-dec7.txt"))))))))

(re-matches regex "Step C must be finished before step A can begin.")
(sort-by first (distinct (flatten  [["C" "A"] ["C" "F"]]))), "A" [["A" "B"] ["A" "D"]], "B" [["B" "E"]], "D" [["D" "E"]], "F" [["F" "E"]]

(:C "A" "F")

(map second (name :A))

(defn enabled
  "Which step to take first?"
  [input]
  (let [steps (set (map first input))
        dependencies (set (map keyword (distinct (mapcat second input))))
        enabled (set/difference steps dependencies)]
    (if (< 1 (count enabled))
      (first (sort enabled))
      (first enabled))))

(keyword (enabled input-small))
;sort on keywords, nice!
(sort (enabled input))

(set (map keyword (distinct (mapcat second [[:C ["A" "F"]] [:A ["B" "D"]] [:B ["E"]] [:D ["E"]] [:F ["E"]]]))))

(set (map first [[:C ["A" "F"]] [:A ["B" "D"]] [:B ["E"]] [:D ["E"]] [:F ["E"]]]))

(defn execute-step
  [step
   remaining-steps]
  (filter #(not (= step (first %))) remaining-steps))

(enabled (execute-step #{:F} (enabled (execute-step :D (execute-step :B (execute-step :A (execute-step :C input-small)))))))

(str (name (first (first input-small))) (str/join (second (first input-small))))

(str/join (second (first input-small)))

(empty? #{:A})

(< 1 (count #{:D :C :B}))

(defn plan
  [input]
  (loop [remaining-steps input
         so-far ""]
    (if (empty? remaining-steps)
      so-far
      (let [enabled (enabled remaining-steps)
            remaining (execute-step enabled remaining-steps)]
        (if (empty? remaining)
          (str so-far (name (first (first remaining-steps))) (str/join (second (first remaining-steps))))
          (recur remaining (str so-far (name enabled))))))))

;"IBJTUWGFKDNVEYAHOMPCQRLSZX"
(= "CABDFE" (plan input-small))

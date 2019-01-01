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
  ;transform the input to a data structure suitable for the algorithm
  (map (fn [[step & dependencies]] [step dependencies])
       (map (fn [step] (distinct (flatten (second step))))
            (group-by first (map (fn [matches] [(keyword (second matches)) (last matches)]) (map (partial re-matches regex) (str/split-lines (slurp "src/advent_of_code_2018/input-dec7.txt"))))))))

(re-matches regex "Step C must be finished before step A can begin.")
(sort-by first (distinct (flatten  [["C" "A"] ["C" "F"]]))), "A" [["A" "B"] ["A" "D"]], "B" [["B" "E"]], "D" [["D" "E"]], "F" [["F" "E"]]

(defn enabled
  "Which step to take first?"
  [input]
  (let [steps (set (map first input))
        dependencies (set (map keyword (distinct (mapcat second input))))
        ;this is the essence of the algorithm
        enabled (set/difference steps dependencies)]
    (if (< 1 (count enabled))
      ;of more than one step is enabled, sort alphabetically first
      (first (sort enabled))
      (first enabled))))

(set (map keyword (distinct (mapcat second [[:C ["A" "F"]] [:A ["B" "D"]] [:B ["E"]] [:D ["E"]] [:F ["E"]]]))))

(set (map first [[:C ["A" "F"]] [:A ["B" "D"]] [:B ["E"]] [:D ["E"]] [:F ["E"]]]))

(defn execute-step
  "Removes a `step` keyword from a list and returns the `remaining-steps`."
  [step
   remaining-steps]
  (filter #(not (= step (first %))) remaining-steps))

;demonstrates how a simple DSL can be built
;(enabled (execute-step :F (enabled (execute-step :D (execute-step :B (execute-step :A (execute-step :C input-small)))))))

(defn plan
  "Determines the order in which `input` steps should be completed."
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

(= "CABDFE" (plan input-small))

;"IBJTUWGFKDNVEYAHOMPCQRLSZX"
(= "IBJTUWGFKDNVEYAHOMPCQRLSZX" (plan input))

(int \A)

(defn time-it-takes
  "Takes a `step` keyword and returns it with the time it takes to execute."
  [extra-time
   step]
  (let [bytes-array (.getBytes (name step))]
    [step (+ 60 (+ extra-time (- (+ (first bytes-array) 1) 65)))]))

(time-it-takes 0 :C)

(defn multiple-enabled
  "Which step(s) to take first considering a set of `curently-executing` steps?"
  [input
   currently-executing]
  (let [steps (set (map first input))
        dependencies (set (map keyword (distinct (mapcat second input))))
        ;this is the essence of the algorithm
        enabled (set/difference steps (set/union dependencies currently-executing))]
    (sort enabled)))

(set/union (set [:A :B]) (set [:C]))

(defn execute-multiple-steps
  "Removes a list of `steps` keywords from a list and returns the `remaining-steps`."
  [steps
   remaining-steps]
  (filter #(not (contains? (set steps) (first %))) remaining-steps))

(defn tick
  "Decreases `busy-workers` time by one and removes workers that finished their task."
  [busy-workers]
  (filter (fn [[_ time-busy]] (< 0 time-busy))
          (map (fn [[worker time-busy]] [worker (dec time-busy)]) busy-workers)))

(defn get-step-dependencies
  [input
   step]
  (map keyword (second (first (filter #(= step (first %)) input)))))

(filter #(= :I (first %)) input)

(get-step-dependencies input :I)

(set (mapcat (partial get-step-dependencies input) [:A :C]))

(defn schedule
  "Determines how long it takes to complete all `input` steps given a number of `workers`."
  [input
   workers]
  (loop [remaining-steps input
         busy-workers []
         total-execution-time 0]
    (if (and (empty? remaining-steps) (empty? busy-workers))
      total-execution-time
      ;assign a new step only if there are workers available
      (let [busy-count (count busy-workers)
            busy-worker-steps (set (mapcat (partial get-step-dependencies input) (vec (map first busy-workers))))
            steps-to-execute (take (- workers busy-count) (multiple-enabled remaining-steps busy-worker-steps))
            new-workers (map (partial time-it-takes 0) steps-to-execute)
            remaining (execute-multiple-steps steps-to-execute remaining-steps)
            latest-schedule (concat busy-workers new-workers)
            last-to-finish (second (last (sort-by second latest-schedule)))]
        (if (and (empty? remaining) (= 1 (count remaining-steps)))
          ;the step before the last, schedule the last as well
          (recur remaining (tick (concat latest-schedule [(time-it-takes last-to-finish (keyword (first (second (first remaining-steps)))))])) (inc total-execution-time))
          (recur remaining (tick latest-schedule) (inc total-execution-time)))))))

;1118
(schedule input 5)

(def busy-workers
  [[:C 3] [:D 14]])

(vec (map first busy-workers))

(tick (tick (tick (tick busy-workers))))

(def busy
  (map (fn [[worker _]] [worker (list (name worker))]) busy-workers))

(= 15 (schedule input-small 2))
(= 21 (schedule input-small 1))

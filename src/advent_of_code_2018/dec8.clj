(ns advent-of-code-2018.dec8
  (:require [clojure.string :as str]))

(def license [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(def license-2 [2 3 1 1 0 1 99 2 0 3 10 11 12 1 1 2])

(second license)

(apply + (take-last (second license) license))

(drop 2 (drop-last 3 license))

(def history [[3 6] [1 1] [2 3] [2 5]])

(vec (drop-last 2 history))

(conj (drop-last 2 [[3 6] [1 1] [2 3] [2 5]]) [1 3])

(conj (vec (drop-last 2 history)) [(dec (first (second (reverse history)))) (second (second (reverse history)))])

(defn mark-as-read
  "Drops the last entry from the history and marks a child as read in the parent entry."
  [history]
  (if (and (= 2 (count history)) (= 0 (first (first history))))
    []
    (conj (vec (drop-last 2 history)) [(dec (first (second (reverse history)))) (second (second (reverse history)))])))

(mark-as-read history)

(mark-as-read [[1 3]])

(defn get-meta
  [license]
  (loop [history [[(first license) (second license)]]
         rest-of-license (drop 2 license)
         sum 0]
    (if (empty? history)
      sum
      (let [child-nodes (first (last history))
            metadata-entries (second (last history))]
        (if (= 0 child-nodes)
          (recur (mark-as-read history) (drop metadata-entries rest-of-license) (+ sum (apply + (take metadata-entries rest-of-license))))
          (recur (conj history [(first rest-of-license) (second rest-of-license)]) (drop 2 rest-of-license) sum))))))

(get-meta license)

(def input (map read-string (str/split (slurp "src/advent_of_code_2018/input-dec8.txt") #" ")))

;36027, solved P1
(get-meta input)

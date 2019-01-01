(ns advent-of-code-2018.dec16
  (:require [clojure.string :as str]))

(def input-small "Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]

Before: [1, 1, 1, 0]
4 1 0 0
After:  [1, 1, 1, 0]

Before: [1, 1, 0, 1]
5 1 3 3
After:  [1, 1, 0, 1]

Before: [3, 2, 3, 1]
14 2 1 3
After:  [3, 2, 3, 2]")

(partition 3 (filter #(not (= "" %)) (str/split-lines input-small)))

(re-matches #"Before: \[(\d*)\,\s(\d*)\,\s(\d*)\,\s(\d*)\]" "Before: [3, 2, 3, 1]")
(re-matches #"(\d*)\s(\d*)\s(\d*)\s(\d*)" "14 2 1 3")
(re-matches #"After:  \[(\d*)\,\s(\d*)\,\s(\d*)\,\s(\d*)\]" "After:  [3, 2, 2, 1]")

(eval (read-string "[3 2 1 (+ 3 7)]"))

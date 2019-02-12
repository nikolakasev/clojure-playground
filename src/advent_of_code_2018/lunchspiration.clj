(ns advent-of-code-2018.lunchspiration
  (:require [clojure.string :as str]))

(def why-clojure?
  "Paul Graham's essay 'Beating the Averages'")

(def why-advent-of-code?
  "Do I still have what it takes?")

;basic clojure
(+ 1 2)
(+ 1 2 3 4 5 6)
((comp inc *) 2 3)

(* 2 4)

(contains? [1 2 6 8 9] 3)

(def add10 (partial + 10))

(add10 5)

;data structures

(seq {:key "value"})

(set [1 2 3 4 5 5])

;function

(defn hello
  [name]
  (str "Hello " name))

;function with variable arity &
(defn multi-arity
  [first & rest]
  (str/join ", " rest))

(multi-arity "one" "two" "three" "four")

[1 2 3 4]

;partial

;function composition

;keep for desert

(defmacro backwards
  [form]
  (reverse form))

(reverse [1 2 3 4 5])

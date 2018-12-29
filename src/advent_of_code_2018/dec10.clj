(ns advent-of-code-2018.dec10
  (:require [clojure.string :as str]))

(def line (map read-string (re-seq #"-?\d+" "position=< 3, -2> velocity=<-1,  1>")))

(def point (zipmap [:p-x :p-y :v-x :v-y] line))

(defn fast-forward-point
  [point
   seconds]
  (-> point
      (update :p-x + (* (:v-x point) seconds))
      (update :p-y + (* (:v-y point) seconds))))

(fast-forward-point point 10)

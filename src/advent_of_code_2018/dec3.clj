(ns advent-of-code-2018.dec3)

(def id "#1 @ 1,3: 4x4")
(def id-2 "#2 @ 3,1: 4x4")
(def id-3 "#3 @ 5,5: 2x2")

(re-matches #"#\d*\s@\s(\d*)\,(\d*):\s(\d*)x(\d*)" id)

(System/getProperty "user.dir")
(slurp "src/advent_of_code_2018/input-dec3.txt")

(defn box-coordinates
  [box-id]
  (let [groups (rest (re-matches #"#\d*\s@\s(\d*)\,(\d*):\s(\d*)x(\d*)" box-id))
        r (read-string (second groups))
        c (read-string (first groups))
        wide (read-string (first (reverse groups)))
        tall (read-string (second (reverse groups)))]
    [[r c] [r (+ c wide)] [(+ r tall) c] [(+ r tall) (+ c wide)]]))

(box-coordinates id)
(box-coordinates id-2)

(defn overlap?
  [box-one
   box-two]
  false)

(interleave [1 2] [4 5])

(= true (overlap? id id-2))
(= false (overlap? id id-3))
(= false (overlap? id-2 id-3))

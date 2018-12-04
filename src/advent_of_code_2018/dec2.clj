(ns advent-of-code-2018.dec2
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.data :as data]))

(+ 1 1)

(set ["1a" "2b" "1a"])

(get {:a 0 :b 1} :b)

(into {:a 0 :b 1 :c 9} {:c 8})

(rest {:a 0 :b 1 :c 3})

(frequencies "abababaaa")

((comp keyword str) {\a 6})

(keyword {:a 1})

(frequencies (map (comp keyword str) "ababaa"))

(keyword "a")

(map (fn [ch] (str "c:" ch)) "abcd")

(def negative-quotient (comp - /))

(negative-quotient 8 2)

(val {:a 1})

(= (first (vals {:a 1})) 1)

(apply map vector [[:a :b :c] [:d :e :f]])

(defn occurences
  [string]
  (map (fn [[k v]] (str k v)) (frequencies string)))

(def IDs
  "abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab")

(def puzzle-input (slurp "src/advent_of_code_2018/input-dec2.txt"))

(def puzzle-input-smaller "wkzhyfdpluzeqvajtbbosngkxc
wrzhyfdplumeqvajtbioskfksc
wrzhyfdolumyqvajtbiosngkxs
urzhyfdplbmeqvrjtbiosngkxc")

(defn ends-with-number
  [string]
  (read-string (last (re-matches #"(\w)(\d*)" string))))

(defn two-or-three
  [times
   string]
  (let [occurs (ends-with-number string)]
    (= times occurs)))

(filter (partial two-or-three 2) ["b1", "a3", "a2"])

(defn checksum
  [IDs]
  (* (count (filter not-empty (map (fn [p] (filter (partial two-or-three 2) p)) (map occurences (clojure.string/split-lines IDs)))))
     (count (filter not-empty (map (fn [p] (filter (partial two-or-three 3) p)) (map occurences (clojure.string/split-lines IDs)))))))

(apply + [1 2 3 4 5])

(checksum IDs)

(checksum puzzle-input)

(checksum puzzle-input-smaller)

(apply * [1 2 4 5])

(read-string (last (re-matches #"(\w)(\d*)" "a10")))

(keyword (str \a))

(distinct [1 2 1 3 1 4 1 5])

(zipmap [:a :b :c :d :e] [1 2 3 4 5])

(keys (occurences "abcdef"))

(into #{} (for [a [1 2 3 4]
                b [1 2 3 4]
                :when (not= a b)]
            [a b]))

(data/diff (vec "fghij") (vec "fguij"))
(data/diff (vec "abcde") (vec "axcye"))

(combo/permutations [1 1 2])

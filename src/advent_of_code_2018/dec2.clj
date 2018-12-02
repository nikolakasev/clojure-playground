(ns advent-of-code-2018.dec2)

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
  (frequencies string))

(conj (occurences "abcdef") (occurences "bababc"))

(keyword (str \a))

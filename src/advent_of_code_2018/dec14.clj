(defn positive-numbers
  ([] (positive-numbers 1))
  ([n] (lazy-seq (cons n (positive-numbers (inc n))))))

(take 10 (positive-numbers))

(defn fib
  ([]
   (fib 1 1))
  ([a b]
   (lazy-seq (cons a (fib b (+ a b))))))

(nth (take 5 (fib)) (dec (mod 8 5)))

(take 10 (fib))

(mod 2 2)

(defn digits [number] (vec (map #(Character/digit % 10) (str number))))

(defn chocolate-recipe
  ([] (chocolate-recipe 0 1 2 [3 7]))
  ([one two length so-far] (let [score-one (nth so-far one)
                                 score-two (nth so-far two)
                                 digits (digits (+ score-one score-two))
                                 length' (+ length (count digits))]
                             (lazy-cat digits
                                       (chocolate-recipe (mod (+ one score-one 1) length')
                                                         (mod (+ two score-two 1) length')
                                                         length'
                                                         (apply conj so-far digits))))))

(nth [3 7 1 0 1 0] (mod (+ 1 7 1) 6))

(take 25 (chocolate-recipe))

;solves P1
(take 10 (drop (- 286051 2) (chocolate-recipe)))

(first (chocolate-recipe))

(digits 8)

(= [0 1 2 4 5 1 5 8 9 1] (take 10 (drop 3 (chocolate-recipe))))
(= [9 2 5 1 0 7 1 0 8 5] (take 10 (drop 16 (chocolate-recipe))))
(= [5 9 4 1 4 2 9 8 8 2] (take 10 (drop 2016 (chocolate-recipe))))

(defn choco-search
  [scores]
  (loop [recipes-so-far (chocolate-recipe)
         scores-so-far scores
         last-op-was-match false
         acc 0]
    (if (empty? scores-so-far)
      ;2 because chocolate-recipe doesn't return the first two elements '(3 7)
      (+ acc 2)
      (if (= (first recipes-so-far) (first scores-so-far))
        (recur (rest recipes-so-far) (rest scores-so-far) true acc)
        (if last-op-was-match
          (recur recipes-so-far scores false (+ (- (count scores) (count scores-so-far)) acc))
          (recur (rest recipes-so-far) scores false (inc acc)))))))

(= 9 (choco-search '(5 1 5 8 9)))
(= 5 (choco-search '(0 1 2 4 5)))
(= 18 (choco-search '(9 2 5 1 0)))
(= 2018 (choco-search '(5 9 4 1 4)))

;solves P2"Elapsed time: 197489.438892 msecs"
;(time (choco-search '(2 8 6 0 5 1)))

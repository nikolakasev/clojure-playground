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
  ([one two length so-far] (let [digits (digits (+ (nth so-far (mod one length))
                                                   (nth so-far (mod two length))))]
                             (lazy-cat so-far
                                       digits
                                       (chocolate-recipe (inc one)
                                                         (inc two)
                                                         (+ length (count digits))
                                                         (concat so-far digits))))))

(nth [3 7 1 0] (mod 5 4))

(take 10 (chocolate-recipe))

(digits 8)

(= [0 1 2 4 5 1 5 8 9 1] (take 10 (drop 5 (chocolate-recipe [3 7] 1 2 2))))
(= [9 2 5 1 0 7 1 0 8 5] (take 10 (drop 2018 (chocolate-recipe [3 7] 1 2 2))))

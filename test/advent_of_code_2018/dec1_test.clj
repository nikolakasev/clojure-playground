(ns advent-of-code-2018.dec1-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2018.dec1 :as d]
            [advent-of-code-2018.dec15 :as d15]))

;run a single test `lein test :only advent-of-code-2018.dec1-test/a-test`
(deftest a-test
  (testing "First puzzle: calculate frequencies correctly"
    (is (= (d/end-frequency
            "+1
-2
+3
+1") 3))
    (is (= (d/end-frequency
            "+1
+1
+1") 3))
    (is (= (d/end-frequency
            "+1
+1
-2") 0))
    (is (= (d/end-frequency
            "-1
-2
-3") -6))))

(deftest d15-input-one
  (testing "First puzzle: first example"
    (is (= 27730 (d15/battle "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")))))

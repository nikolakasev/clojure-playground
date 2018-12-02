(ns advent-of-code-2018.dec1-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2018.dec1 :as d]))

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

;(deftest b-test
;  (testing "Second puzzle: reach the same frequency twice"
;    (is (= ()))))

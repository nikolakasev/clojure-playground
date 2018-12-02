(ns advent-of-code-2018.dec1-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2018.dec1]))

(deftest a-test
  (testing "First puzzle: calculate frequencies correctly"
    (is (= (advent-of-code-2018.dec1/end-frequency
            "+1
-2
+3
+1") 3))
    (is (= (advent-of-code-2018.dec1/end-frequency
            "+1
+1
+1") 3))
    (is (= (advent-of-code-2018.dec1/end-frequency
            "+1
+1
-2") 0))
    (is (= (advent-of-code-2018.dec1/end-frequency
            "-1
-2
-3") -6))))

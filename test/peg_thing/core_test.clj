(ns peg-thing.core-test
  (:require [clojure.test :refer :all]
            [peg-thing.core :refer :all]))

(deftest tri-tests
  (testing "Given an int, returns the triangular numbers to that limit"
    (is (= [1 3 6 10 15] (take 5 tri)))))

(deftest triangular?-tests
  (is (triangular? 10))
  (is (not (triangular? 16))))

(ns peg-thing.core-test
  (:require [clojure.test :refer :all]
            [peg-thing.core :refer :all]))

(deftest tri-tests
  (testing "Given an int, returns the triangular numbers to that limit"
    (is (= [1 3 6 10 15] (take 5 tri)))))

(deftest triangular?-tests
  (is (triangular? 10))
  (is (not (triangular? 16))))

(deftest row-tri-tests
  (testing "Given a row number, returns the triangular number at the end of the row"
    (is (= 6 (row-tri 3)))
    (is (= 10 (row-tri 4)))))

(deftest row-num-tests
  (testing "Given a position, returns the row number in which that position appears"
    (is (= 2 (row-num 2)))
    (is (= 3 (row-num 5)))
    (is (= 4 (row-num 9)))))

(deftest connect-tests?
  (testing "Given a set of valid inputs, connects the pos with the neighbor and jump destination"
    (= {1 {:connections {4 2}}
        4 {:connections {1 2}}}
       (connect {} 15 1 2 4)))
  (testing "Given a set of invalid inputs, things"
    (= nil (connect {} 15 7 16 22))))
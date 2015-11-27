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
    (is (= {1 {:connections {4 2}}
            4 {:connections {1 2}}}
           (connect {} 15 1 2 4))))
  (testing "Given a set of invalid inputs, things"
    (is (= {} (connect {} 15 7 16 22)))))

(deftest connect-right-tests
  (testing "Given available connection to right, connects"
    (is (= {4 {:connections {6 5}}
            6 {:connections {4 5}}} (connect-right {} 15 4)))
    (is (= {8 {:connections {10 9}}
            10 {:connections {8 9}}} (connect-right {} 15 8))))
  (testing "Given a pos with no available destination, does not connect"
    (is (= {} (connect-right {} 15 9)))
    (is (= {} (connect-right {} 15 10)))))

(deftest connect-down-left-tests
  (testing "Given available connection down and left, connects"
    (is (= {3 {:connections {8 5}}
            8 {:connections {3 5}}}
           (connect-down-left {} 15 3)))
    (is (= {6 {:connections {13 9}}
            13 {:connections {6 9}}}
           (connect-down-left {} 15 6))))
  (testing "Given a pos with no available destination, does not connect"
    (is (= {} (connect-down-left {} 15 9)))))

(deftest connect-down-right-tests
  (testing "Given available connection down and right, connects"
    (is (= {2 {:connections {9 5}}
            9 {:connections {2 5}}}
           (connect-down-right {} 15 2)))
    (is (= {6 {:connections {15 10}}
            15 {:connections {6 10}}}
           (connect-down-right {} 15 6))))
  (testing "Given a pos with no available destination, does not connect"
    (is (= {} (connect-down-right {} 15 8)))))

(deftest add-pos-tests
  (is (= {1 {:connections {6 3 4 2} :pegged true}
          4 {:connections {1 2}}
          6 {:connections {1 3}}}
         (add-pos {} 15 1))))

(deftest new-board-tests
  (is (= {:rows 4
          1 {:connections {4 2 6 3} :pegged true}
          2 {:connections {7 4 9 5} :pegged true}
          3 {:connections {8 5 10 6} :pegged true}
          4 {:connections {1 2 6 5} :pegged true}
          5 {:pegged true}
          6 {:connections {1 3 4 5} :pegged true}
          7 {:connections {2 4 9 8} :pegged true}
          8 {:connections {3 5 10 9} :pegged true}
          9 {:connections {2 5 7 8} :pegged true}
          10 {:connections {3 6 8 9} :pegged true}}
         (new-board 4))))

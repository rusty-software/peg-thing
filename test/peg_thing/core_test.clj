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
  (testing "given a board, max-pos and position, returns the board with position pegged and connections made"
    (is (= {1 {:connections {6 3 4 2} :pegged true}
            4 {:connections {1 2}}
            6 {:connections {1 3}}}
           (add-pos {} 15 1)))))

(deftest new-board-tests
  (testing "Given a rowcount, returns a board with rows key and fully pegged connections"
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
           (new-board 4)))))

(let [board {:rows 4
             1 {:connections {4 2 6 3} :pegged true}
             2 {:connections {7 4 9 5} :pegged true}
             3 {:connections {8 5 10 6} :pegged true}
             4 {:connections {1 2 6 5}}
             5 {:pegged true}
             6 {:connections {1 3 4 5} :pegged true}
             7 {:connections {2 4 9 8} :pegged true}
             8 {:connections {3 5 10 9} :pegged true}
             9 {:connections {2 5 7 8} :pegged true}
             10 {:connections {3 6 8 9} :pegged true}}]

  (deftest pegged?-tests
    (testing "Given a board and a pos, returns true if pegged; otherwise, false"
      (is (pegged? board 3))
      (is (not (pegged? board 4)))))

  (deftest remove-peg-tests
    (testing "Given a board and position, returns a new board with the position unpegged"
      (is (get-in board [3 :pegged]))
      (let [unpegged-board (remove-peg board 3)]
        (is (not (get-in unpegged-board [3 :pegged]))))))

  (deftest place-peg-tests
    (testing "Given a board and position, returns a new board with the position pegged"
      (is (not (get-in board [4 :pegged])))
      (let [pegged-board (place-peg board 4)]
        (is (get-in pegged-board [4 :pegged])))))

  (deftest move-peg-tests
    (testing "Given a board and two positions, returns a board with first position unpegged and second position pegged"
      (is (get-in board [1 :pegged]))
      (is (not (get-in board [4 :pegged])))
      (let [moved-board (move-peg board 1 4)]
        (is (not (get-in moved-board [1 :pegged])))
        (is (get-in moved-board [4 :pegged]))))))

(let [board {:rows 5
             1 {:connections {6 3, 4 2}, :pegged true}
             2 {:connections {9 5, 7 4}, :pegged true}
             3 {:connections {10 6, 8 5}, :pegged true}
             4 {:pegged false, :connections {13 8, 11 7, 6 5, 1 2}} ;; unpegged
             5 {:connections {14 9, 12 8}, :pegged true}
             6 {:pegged false, :connections {15 10, 13 9, 4 5, 1 3}} ;; unpegged
             7 {:pegged true, :connections {9 8, 2 4}}
             8 {:pegged true, :connections {10 9, 3 5}}
             9 {:pegged true, :connections {7 8, 2 5}}
             10 {:pegged true, :connections {8 9, 3 6}}
             11 {:pegged true, :connections {13 12, 4 7}}
             12 {:pegged true, :connections {14 13, 5 8}}
             13 {:pegged true, :connections {15 14, 11 12, 6 9, 4 8}}
             14 {:pegged true, :connections {12 13, 5 9}}
             15 {:pegged true, :connections {13 14, 6 10}}}]

  (deftest valid-moves-tests
    (testing "Given a board and pos, returns a map of valid moves for the position, with jumped position as the key and destination as the value"
      (is (= {4 2 6 3} (valid-moves board 1)))
      (is (= {4 7} (valid-moves board 11))))
    (testing "Given a board and pos with no valid moves, returns an empty map"
      (is (= {} (valid-moves board 5)))
      (is (= {} (valid-moves board 8)))))

  (deftest valid-move?-tests
    (testing "Given a board and two positions, returns the jumped position if move is valid; otherwise, nil"
      (is (= 2 (valid-move? board 1 4)))
      (is (= 3 (valid-move? board 1 6)))
      (is (= 7 (valid-move? board 11 4)))
      (is (nil? (valid-move? board 12 5)))))

  (deftest make-move-tests
    (testing "Given a board and two positions, returns new board with position 1 and jumped unpegged, position 2 pegged"
      (let [new-board (-> board
                          (update-in [1 :pegged] (constantly false))
                          (update-in [2 :pegged] (constantly false))
                          (update-in [4 :pegged] (constantly true)))]
        (is (= new-board (make-move board 1 4))))
      (is (= board (make-move board 12 5)))))

  (deftest can-move?-tests
    (testing "Given a board with valid moves, returns the first map of valid moves; otherwise, nil"
      (is (= {4 2 6 3} (can-move? board)))
      (let [new-board (-> board
                          (update-in [4 :pegged] (constantly true))
                          (update-in [6 :pegged] (constantly true)))]
        (is (nil? (can-move? new-board))))))

  (deftest render-pos-tests
    (testing "Renders lettered position with 0 for pegged and - for unpegged"
      (is (= "c0" (render-pos board 3)))
      (is (= "d-" (render-pos board 4)))))

  (deftest row-positions-tests
    (testing "Given a row num, returns a seq of the positions in the row"
      (is (= [1] (row-positions 1)))
      (is (= [2 3] (row-positions 2)))
      (is (= [4 5 6] (row-positions 3)))
      (is (= [7 8 9 10] (row-positions 4)))
      (is (= [11 12 13 14 15] (row-positions 5)))))

  (deftest row-padding-tests
    (testing "Given a row number and total row count, returns a string with spaces appropriate to center the row"
      (is (= "" (row-padding 5 5)))
      (is (= "  " (row-padding 4 5)))
      (is (= "   " (row-padding 3 5)))
      (is (= "     " (row-padding 2 5)))
      (is (= "      " (row-padding 1 5)))))

  (deftest render-row-tests
    (testing "Given a board and row num, generates the string representation of the row"
      (is (= "      a0" (render-row board 1)))
      (is (= "     b0 c0" (render-row board 2)))
      (is (= "   d- e0 f-" (render-row board 3)))
      (is (= "  g0 h0 i0 j0" (render-row board 4)))
      (is (= "k0 l0 m0 n0 o0" (render-row board 5))))))


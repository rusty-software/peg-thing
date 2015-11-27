(ns peg-thing.core
  (:require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn tri*
  "Generate lazy seq of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
    (let [new-sum (+ sum n)]
      (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Returns true if the given value is triangular (1, 3, 6, 10, etc), false otherwise"
  [n]
  (= n (last (take-while #(<= % n) tri))))

(defn row-tri
  "Returns the triangular number at the end of the given row number"
  [row]
  (last (take row tri)))

(defn row-num
  "Returns the row number in which the given position appears"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  "Creates connections between a given position and a position to the right"
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  "Creates connections between a given position and a position down and left"
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  "Creates connections between a given position and a position down and right"
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  "Adds a position and all connected positions to the board, marking the position as pegged"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Initializes a new board to a row count"
  [rowcount]
  (let [initial-board {:rows rowcount}
        max-pos (row-tri rowcount)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Removes the peg from the given position"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Places a peg in the given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Removes a peg from the first position and places it in the second position"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Returns a map of valid moves given a board and starting position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))

(defn valid-move?
  "Returns the jumped position for a valid move, nil otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Given a board and two positions, returns new board with position 1 and jumped position unpegged, position 2 pegged; otherwise, board"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)
    board))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

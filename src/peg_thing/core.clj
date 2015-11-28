(ns peg-thing.core
  (:require [clojure.string :as str])
  (:gen-class))

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
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Given a board with valid moves, returns the first map of valid moves; otherwise, nil"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(defn render-pos
  "Render a specific pos as either pegged or unpegged"
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         "0"
         "-")))

(defn row-positions
  "Return all positions for a given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "String of spaces to prepend to a row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (repeat pad-length " "))))

(defn render-row
  "Generates the string representation for a single row given a board"
  [board row-num]
  (str (row-padding row-num (:rows board))
       (str/join " " (map (partial render-pos board) (row-positions row-num)))))

(defn print-board
  "Prints the board to stdout"
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "Converts a letter string to a position number"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input nil))
  ([default]
    (let [input (str/trim (read-line))]
      (if (empty? input)
        default
        (str/lower-case input)))))

(defn characters-as-strings
  "Converts a string of characters to a seq of string characters"
  [letters]
  (map str (str/replace letters " " "")))

(declare prompt-move game-over)

(defn prompt-empty-peg
  "Removes a peg from the board and prompts for the next move"
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  "Initializes a game by prompting for the board row count and then prompting for moves"
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn user-entered-invalid-move
  "Handles next step after user enters invalid move"
  [board]
  (println "\n!!!  That was an invalid move :-(\n")
  (prompt-move board))

(defn user-entered-valid-move
  "Handles next step after user enters a valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn prompt-move
  "Read the player's move and act on it"
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where?  Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn game-over
  "Announces game is over and prompts for another game"
  [board]
  (let [remaining-pegs (count (filter #(:pegged %) (vals board)))
        score-msg (case remaining-pegs
                    1 "You're a Genius!"
                    2 "You're pretty smart!"
                    3 "Not bad, but anybody can leave 3..."
                    "Wow, you should really practice more!")]
    (println "Game over!  You had" remaining-pegs "pegs left.")
    (println score-msg)
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn -main
  [& args]
  (prompt-rows))

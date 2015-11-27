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
  (inc (count (take-while #(<= % pos) tri))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

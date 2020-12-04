(ns aoc2020.day03
  (:require [clojure.java.io :as io]))

(def in-file
  (line-seq (io/reader (io/resource "day3.data"))))

(defn plus-x
  [x]
  #(+ x %))

(def next-x
  #(iterate (plus-x %) 0))

(defn process-line
  [line x]
  (= \# (first (take 1 (drop x (cycle line))))))

(defn p1
  [lines x y]
  (count
   (filter true?
           (map process-line (take-nth y lines) (next-x x)))))

(defn p2
  [lines]
  (*
   (p1 lines 1 1)
   (p1 lines 3 1)
   (p1 lines 5 1)
   (p1 lines 7 1)))

(ns aoc2020.core
  (:require [clojure.java.io :as io]))

(def in-file
  (line-seq (io/reader (io/resource "day3.data"))))

(def line "....#...#####..##.#..##..#....#")

;; ....#...#####..##.#..##..#....#
;; ..##.#.#.........#.#......##...
;; #.#.#.##.##...#.......#...#..#.
;; ..##.............#.#.##.....#..
;; ##......#.............#....#...
;; .....##..#.....##.#.......##..#
;; .##.....#........##...##.#....#


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
   (p1 lines 7 1)
   (p1 lines 1 2)))



(def data
  (clojure.string/split-lines (slurp "./resources/day3.data")))

(defn path [dy dx height width]
  (for [i (range)
        :let [y (* dy i) x (* dx i)]
        :while (< y height)]
    [y (mod x width)]))

(defn count-trees [grid dy dx]
  (->> (path dy dx (count grid) (count (first grid)))
    (filter #(= (get-in grid %) \#))
    count))

(defn part-1 [grid]
  (count-trees grid 1 3))

(defn part-2 [grid]
  (apply * (map #(apply count-trees grid %)
             [[1 1] [1 3] [1 5] [1 7] [2 1]])))


     
(defn -main
  [& args]
  (println "Go to each day file to see solutions, u lazy person"))

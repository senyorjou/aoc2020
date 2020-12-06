(ns aoc2020.day06
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data
  (slurp "./resources/day6.data"))

(defn parse-chunks [data]
   (str/split data #"\n\n"))

(defn parse-lines [lines]
  (map #(str/split % #"\s") lines))

(defn extract-chars [line]
  (apply hash-set (apply str (map str line))))

(defn intersect-chars [line]
  (apply set/intersection (map #(apply hash-set %) line))) 

(defn solve [fn data]
  (->> data
       parse-chunks
       parse-lines
       (map fn)
       (map count)
       (reduce +)))

(defn p1 [data]
  (solve extract-chars data))

(defn p2 [data]
  (solve intersect-chars data))

(ns aoc2020.day02
  (:require [clojure.java.io :as io]))

(def in-file
  (line-seq (io/reader (io/resource "day2-1.data"))))

(defn extract-password-data [password-line]
  (let [password (re-find #"(\d+)-(\d+)\s(.):\s(.+)" password-line)]
    (zipmap [:min-val :max-val :character :text] (rest password))))

(def to-int #(Integer/parseInt %))

(defn match?
  [matches min max]
    (if (and (>= matches min) (<= matches max))
      true
      false))

(defn validate-pass-rule-1
  [{:keys [min-val max-val character text]}]
  (let [c (first character)
        min (to-int min-val)
        max (to-int max-val)
        matches (count (filter #(= c %) text))]
    (match? matches min max)
  ))

(defn validate-pass-rule-2
  [{:keys [min-val max-val character text]}]
  (let [pos-1 (dec (to-int min-val))
        pos-2 (dec (to-int max-val))
        c (first character)]
    (= 1
    (count
     (filter true?
             [
              (= (get text pos-1) c)
              (= (get text pos-2) c)
              ])))))

(defn solver
  [validator lines]
  (count
   (filter true?
           (map #(validator (extract-password-data %))
                lines))))

(defn p1
  [lines]
  (solver validate-pass-rule-1 lines))

(defn p2
  [lines]
  (solver validate-pass-rule-2 lines))

(ns aoc2020.core
  (:require [clojure.java.io :as io]))

(def in-file
  (line-seq (io/reader (io/resource "day1-1.data"))))

(def values
  (map #(Integer/parseInt %) in-file))

;; brute force
(defn brute-p1-1 []
  (set
   (for [a values
         b values
         :when (= 2020 (+ a b))]
     (* a b))))

;; brutest force
(defn brute-p1-2 []
  (set
   (for [a values
         b values
         c values
         :when (= 2020 (+ a b c))]
     (* a b c))))

;; Iterative
(def inv #(- 2020 %))

(defn iter-p1-1 [values]
  (loop [a (first values)
         b (rest values)
         val-set #{}]
    (if (contains? val-set a)
      (* a (inv a))
      (recur (first b) (rest b) (conj val-set (inv a))))))


;; Better, final solution
(defn extract-product [tuple]
  (let [values (second tuple)]
    (* (first values) (second values))))

(defn build-sum-pairs [values fn]
   (for [a values
         b values
         :when (fn 2020 (+ a b))]
     [(+ a b) [a b]]))
  
(defn p1-1 [values]
  (extract-product (first (build-sum-pairs values =))))

(defn p1-2 [values]
  (first
  (for [a (build-sum-pairs values >)
        b values
        :when (= 2020 (+ (first a) b))]
    (* (extract-product a) b))))

;; Second day

(def in-file-2
  (line-seq (io/reader (io/resource "day2-1.data"))))


(defn extract-password-data [password-line]
  (let [password (re-find #"(\d+)-(\d+)\s(.):\s(.+)" password-line)]
    (zipmap [:min-val :max-val :character :text] (rest password))))

(def to-int #(Integer/parseInt %))

(defn validate-pass-rule-1
  [{:keys [min-val max-val character text]}]
  (let [c (first character)
        min (to-int min-val)
        max (to-int max-val)
        matches (count (filter #(= c %) text))]
    (if (and (>= matches min) (<= matches max))
      true
      false)
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

(defn p2-1
  [lines]
  (solver validate-pass-rule-1 lines))

(defn p2-2
  [lines]
  (solver validate-pass-rule-2 lines))

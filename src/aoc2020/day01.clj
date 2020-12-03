(ns aoc2020.day01
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

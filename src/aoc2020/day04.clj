(ns aoc2020.day04
  (:require [clojure.string :as str]))

(def data
  (slurp "./resources/day4.data"))

(defn parse-chunks [data]
   (str/split data #"\n\n"))

(defn parse-lines [lines]
  (map #(str/split % #"\s") lines))

(defn parse-passports [lines]
  (map parse-passport lines))

(defn parse-passport [line]
  (into {} (map #(apply hash-map (str/split % #":")) line)))

(defn is-valid? [passport]
  (let [pass-len (count passport)
        has-cid? (get passport "cid")]
    (or (= pass-len 8)
        (and (= pass-len 7) (not has-cid?)))))

(defn is-valid-pid? [s]
  (and s (re-matches #"\d{9}" s)))

(defn is-valid-ecl? [s]
  (let [valid-colors (set ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])]
    (and s (contains? valid-colors s))))

(defn is-valid-hcl? [s]
  (and s (re-matches #"^#[a-f0-9]{6}" s)))


(defn is-valid-year? [s lo hi]
  (and s
       (re-matches #"\d+" s)
       (<= lo (Integer/parseInt s) hi)))


(defn is-valid-hgt? [s]
  (and s
       (let [[_, height, unit] (re-matches #"(\d+)(..)" s)]
         (case unit
           "in" (<= 59 (Integer/parseInt height) 76)
           "cm" (<= 150 (Integer/parseInt height) 193)
           false))
       ))
       
(defn is-really-valid? [passport]
  (and (is-valid-pid? (passport "pid"))
       (is-valid-ecl? (passport "ecl"))
       (is-valid-hcl? (passport "hcl"))
       (is-valid-year? (passport "byr") 1920 2002)
       (is-valid-year? (passport "iyr") 2010 2020)
       (is-valid-year? (passport "eyr") 2020 2030)
       (is-valid-hgt? (passport "hgt"))
       ))


(defn solve [fn data]
  (count (filter fn (-> data
                       parse-chunks
                       parse-lines
                       parse-passports))))

(defn p1 [data]
  (solve is-valid? data))

(defn p2 [data]
    (solve is-really-valid? data))

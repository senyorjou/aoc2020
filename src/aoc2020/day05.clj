(ns aoc2020.day05
  (:require [clojure.string :as str]))

(def data
  (slurp "./resources/day5.data"))

(defn parse-lines [data]
   (str/split data #"\n"))

(defn parse-item [sample]
  (let [rosseta {\F 0 \B 1 \L 0 \R 1}]
    (read-string (str "2r" (apply str (map rosseta sample))))))

(defn parse-items [sample]
  (let [row (parse-item (take 7 sample))
        seat (parse-item (drop 7 sample))]
    [row seat]))

(defn seat-id [[row seat]]
  (+ (* row 8) seat))

(defn p1 [data]
  (let [lines (parse-lines data)]
      (apply max (map seat-id (map parse-items lines)))))

;; Old trick: substract the sum of a consecutive set with a missing number
;; from the sum of all set and u will get the missing number
;; (sum (1 2 3 4) - sum (1 2 4)) == 3
(defn p2 [data]
  (let [lines (parse-lines data)
        ids (map seat-id (map parse-items lines))
        lo (apply min ids)
        hi (apply max ids)
        all (reduce + (range lo (inc hi)))
        missing (reduce + ids)]
      (- all missing)))

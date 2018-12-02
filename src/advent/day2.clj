(ns advent.day2
  (:require [clojure.string :as str]))

(def input
  (str/split-lines
    (slurp "input/input2.txt")))

(def x
  ["abcdef"
   "bababc"
   "abbcde"
   "abcccd"
   "aabcdd"
   "abcdee"
   "ababab"])

(def y
  ["abcde"
   "fghij"
   "klmno"
   "pqrst"
   "fguij"
   "axcye"
   "wvxyz"])

(defn part1
  [input]
  (let [m (->> input
               (map #(-> % frequencies vals frequencies))
               (mapcat keys)
               frequencies)]
    (* (m 2 0)
       (m 3 0))))

(defn diff
  [a b]
  (->> (map not= a b)
       (filter true?)
       count))

(defn similar
  [a b]
  (->> (map #(when (= %1 %2) %1) a b)
       (keep identity)))

(defn part2
  [input]
  (let [v (first (for [a input b input
                       :when (= 1 (diff a b))]
                   [a b]))]
    (str/join (similar (first v) (second v)))))

(part1 x)
(part1 input)

(part2 y)
(part2 input)

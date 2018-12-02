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
               (mapcat (comp keys frequencies vals frequencies))
               frequencies)]
    (* (m 2 0)
       (m 3 0))))

(defn diff
  [a b]
  (->> (map vector a b)
       (filter (partial apply not=))
       count))

(defn similar
  [a b]
  (->> (map vector a b)
       (filter (partial apply =))
       (map first)))

(defn part2
  [input]
  (->> (for [a input b input
             :when (= 1 (diff a b))]
         [a b])
       first
       (apply similar)
       (apply str)))

(part1 x)
(part1 input)

(part2 y)
(part2 input)

(ns advent.day5
  (:require [clojure.string :as str]))

(def x "dabAcCaCBAcCcaDA")

(def input (str/trim-newline (slurp "input/input5.txt")))

(defn rf
  [[x & more :as coll] y]
  (cond
    (nil? x) [y]
    (#{32 -32} (- x y)) more
    :else (cons y coll)))

(defn reactor
  [coll]
  (reverse
    (reduce rf nil coll)))

(defn react
  [coll]
  (->> coll
       (iterate reactor)
       (partition 2)
       (drop-while (fn [[a b]] (not= a b)))
       ffirst
       count))

(defn clean
  [c coll]
  (remove #{(int c) (+ (int c) 32)} coll))

(defn part1
  [input]
  (react (map int input)))

(defn part2
  [input]
  (reduce min (map #(->> (map int input)
                         (clean %)
                         (react)) uc)))

;;(part1 input)
;;(part2 input)

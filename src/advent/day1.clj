(ns advent.day1
  (:require [clojure.string :as str]))

(def input
  (map #(Integer/parseInt %)
       (str/split-lines (slurp "input/input1.txt"))))

(defn part1
  [input]
  (reduce + input))

(defn f
  [{:keys [seen freq twice] :as m} v]
  (let [f2 (+ freq v)]
    (-> m
        (assoc :freq f2)
        (update :seen conj f2)
        (assoc :twice (or twice (when (seen f2) f2))))))

(defn part2
  [input]
  (->> (cycle input)
       (reductions f {:seen #{0} :freq 0 :twice nil})
       (map :twice)
       (drop-while nil?)
       first))

(part1 input)
(part2 input)

(ns advent.day4
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [clj-time.core :as t]
            [java-time :as jt]))

(def x ["[1518-11-01 00:00] Guard #10 begins shift"
        "[1518-11-01 00:05] falls asleep"
        "[1518-11-01 00:25] wakes up"
        "[1518-11-01 00:30] falls asleep"
        "[1518-11-01 00:55] wakes up"
        "[1518-11-01 23:58] Guard #99 begins shift"
        "[1518-11-02 00:40] falls asleep"
        "[1518-11-02 00:50] wakes up"
        "[1518-11-03 00:05] Guard #10 begins shift"
        "[1518-11-03 00:24] falls asleep"
        "[1518-11-03 00:29] wakes up"
        "[1518-11-04 00:02] Guard #99 begins shift"
        "[1518-11-04 00:36] falls asleep"
        "[1518-11-04 00:46] wakes up"
        "[1518-11-05 00:03] Guard #99 begins shift"
        "[1518-11-05 00:45] falls asleep"
        "[1518-11-05 00:55] wakes up"])

(insta/defparser p
  "<S>         = timestamp <' '> (begins | falls | wakes)
   timestamp = <'['> year <'-'> month <'-'> day <' '> hour <':'> minute <']'>
   begins    = <'Guard #'> num <' begins shift'>
   falls     = <'falls asleep'>
   wakes     = <'wakes up'>
   guard     = num
   <year>      = num
   <month>     = num
   <day>       = num
   <hour>      = num
   <minute>    = num
   num       = #'[0-9]+'")

(defn parse
  [s]
  (->> (p s)
       (insta/transform {:num #(Integer/parseInt %)
                         })))

(def input
  (->> (slurp "input/input4.txt")
       (str/split-lines)
       (map parse)
       (sort-by first)))

(defn stepper
  [{:keys [guard falls] :as acc} [ts [what n]]]
  (condp = what
    :begins (assoc acc :guard n)
    :falls (assoc acc :falls ts)
    :wakes (update acc guard concat (range (last falls) (last ts)))))

(defn part1
  [input]
  (let [guards (dissoc (reduce stepper {} input) :guard :falls)
        selected (->> guards
                      (sort-by (comp - count second))
                      first)
        id (first selected)
        minute (->> (second selected)
                    frequencies
                    (sort-by (comp - second))
                    ffirst)]
    (* id minute)))

(defn part2
  [input]
  (let [selected (->> (dissoc (reduce stepper {} input) :guard :falls)
                      (sort-by (comp - (partial apply max) vals frequencies second))
                      first)
        id (first selected)
        minute (->> (second selected)
                    frequencies
                    (sort-by (comp - second))
                    ffirst)]
    (* id minute)))

(part1 (map parse x))
(part2 (map parse x))

(part1 input)
(part2 input)

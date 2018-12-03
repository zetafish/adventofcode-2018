(ns advent.day3
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [clojure.set :as set]))


(insta/defparser p
  "<S>    = <'#'> id <' @ '> left <','> top <': '> width <'x'> height
   id     = num
   left   = num
   top    = num
   right  = num
   width  = num
   height = num
   num    = #'[0-9]+'")

(defn parse
  [s]
  (into {}
        (insta/transform
          {:num read-string}
          (p s))))

(def input
  (->> (slurp "input/input3.txt")
       (str/split-lines)
       (map parse)))

(defn squares
  [{:keys [id left top width height]}]
  (for [x (range width)
        y (range height)]
    [id (+ left x) (+ top y)]))

(defn f
  [state [id x y]]
  (update state [x y] conj id))

(defn part1
  [input]
  (let [m (->> input
               (mapcat squares)
               (reduce f {})
               (vals)
               (map count)
               (frequencies))]
    (->> (dissoc m 1)
         vals
         (apply +))))

(defn g
  [{:keys [tainted pure] :as acc} ids]
  (if (= 1 (count ids))
    (if (tainted (first ids))
      acc
      (update acc :pure conj (first ids)))
    (-> acc
        (update :tainted set/union (set ids))
        (update :pure set/difference (set ids)))))

(defn part2
  [input]
  (->> input
       (mapcat squares)
       (reduce f {})
       vals
       distinct
       (reduce g {:tainted #{} :pure #{}})
       :pure))

(part2 input)

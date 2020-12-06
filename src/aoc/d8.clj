(ns aoc.d8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def raw (str/trim-newline (slurp (io/resource "d8.txt"))))

(def pixels (map #(Integer/parseInt (str %)) raw))

(def layers (partition (* 25 6) pixels))

(defn count-n
  [n layer]
  (count (filter #(= n %) layer)))

(defn part1
  [pixels w h]
  (->> (partition (* w h) pixels)
       (sort-by (partial count-n 0))
       first
       ((juxt (partial count-n 1)
              (partial count-n 2)))
       (apply *)))

(println (part1 pixels 25 6))

(defn part2
  [pixels w h]
  (->> (partition (* w h) pixels)
       (apply map vector)
       (map (partial drop-while #(= 2 %)))
       (map first)
       (partition w)
       (map (partial map {1 "#" 0 " "}))
       (map str/join)
       (str/join "\n")))

(println (part2 pixels 25 6))

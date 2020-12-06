(ns aoc.d1
  (:require [clojure.java.io :as io]))

(def input
  (let [lines (with-open [r (io/reader (io/resource "d1.txt"))]
                (doall (line-seq r)))]
    (map #(Integer/parseInt %) lines)))

(defn fuel1
  [mass]
  (- (quot mass 3) 2))

(defn fuel2
  [mass]
  (loop [total 0 mass mass]
    (let [x (fuel1 mass)]
      (if (neg? x)
        total
        (recur (+ total x) x)))))

(defn solve [f]
  (fn [input]
    (->> input
         (map f)
         (reduce +))))

((solve fuel1) input)
((solve fuel2) input)

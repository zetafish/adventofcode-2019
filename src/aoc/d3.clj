(ns aoc.d3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse
  [s]
  (map (fn [[c & n]]
         [(keyword (str c))
          (Integer/parseInt (apply str n))])
       (str/split s #",")))

(def input
  (->> (slurp (io/resource "d3.txt"))
       str/split-lines
       (mapv parse)))

(def dir
  {:U [0 1]
   :D [0 -1]
   :L [-1 0]
   :R [1 0]})

(defn mdist
  [[x y]]
  (+ x y))

(defn follow
  [from [d n]]
  (take (inc n) (iterate #(map + (dir d) %) from)))

(defn trace
  [codes]
  (reduce (fn [trace x]
            (->> (follow (last trace) x)
                 (drop 1)
                 (into trace)))
          [[0 0]]
          codes))

(defn solve1 [v1 v2]
  (->>
   (set/intersection (set (drop 1 (trace v1)))
                     (set (drop 1 (trace v2))))
   (map (fn [[x y]] (+ (Math/abs x) (Math/abs y))))
   (sort)
   (first)))

(defn solve2 [v1 v2]
  (let [t1 (drop 1 (trace v1))
        t2 (drop 1 (trace v2))
        points (set/intersection (set t1) (set t2))
        c1 (zipmap t1 (iterate inc 1))
        c2 (zipmap t2 (iterate inc 1))]
    (->>
     points
     (reduce #(assoc %1 %2 (+ (c1 %2) (c2 %2))) {})
     (sort-by second)
     first)))

(solve1 (parse "R8,U5,L5,D3")
        (parse "U7,R6,D4,L4"))

(solve1 (parse "R75,D30,R83,U83,L12,D49,R71,U7,L72")
        (parse "U62,R66,U55,R34,D71,R55,D58,R83"))

(solve1 (parse "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
        (parse "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))

(solve1 (first input) (second input))

(solve2 (parse "R8,U5,L5,D3")
        (parse "U7,R6,D4,L4"))

(solve2 (parse "R75,D30,R83,U83,L12,D49,R71,U7,L72")
        (parse "U62,R66,U55,R34,D71,R55,D58,R83"))

(solve2 (parse "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
        (parse "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))

(solve2 (first input) (second input))

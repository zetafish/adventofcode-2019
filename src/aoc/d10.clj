(ns aoc.d10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def clear \.)
(def asteroid \#)

(defn world [grid]
  (let [mx (count (first grid))
        my (count grid)]
    {:grid grid
     :mx mx
     :my my
     :points (for [x (range mx)
                   y (range my)] [x y])}))

(def x1 (world [".#..#"
                "....."
                "#####"
                "....#"
                "...##"]))

(def input (world (str/split-lines (slurp (io/resource "d10.txt")))))

;; step 1: determine the rays to follow
;; step 2: follow the rays and see if they hit yes or no

;; brute force approach

;; 0,0: top-left
;; 1,0: one right from top-left
;; 0,1: one down from top-left

(defn dist [v1 v2]
  (->> (map - v1 v2)
       (map #(* % %))
       (reduce +)
       Math/sqrt))

(defn inside [{:keys [mx my]} [x y]]
  (and (<= 0 x (dec mx))
       (<= 0 y (dec my))))

(defn see [world [x y]]
  (get-in world [:grid y x]))

(defn trace-ray
  [world from to]
  (let [v (map - to from)]
    (->> (iterate (partial map + v) to)
         (take-while (partial inside world)))))

(defn sweep [world origin]
  (loop [points (->> (:points world)
                     (remove #{origin})
                     (sort-by (partial dist origin)))
         hits []]
    (if (empty? points)
      hits
      (let [ray (trace-ray world origin (first points))
            hit (->> ray
                     (drop-while #(= \. (see world %)))
                     first)
            points (remove (set ray) points)]
        (recur (remove (set ray) points)
               (if hit (conj hits hit) hits))))))

(defn sweep-all
  [world]
  (->> (:points world)
       (filter #(= \# (see world %)))
       (map (fn [origin]
              {:origin origin
               :sweep (sweep world origin)}))))

(last (sort-by (comp count :sweep) (sweep-all x1)))

;; part 1
(count
 (:sweep
  (last (sort-by (comp count :sweep) (sweep-all input)))))

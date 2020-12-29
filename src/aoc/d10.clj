(ns aoc.d10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def clear \.)
(def asteroid \#)

(defn world [grid]
  (let [mx (count (first grid))
        my (count grid)]
    {:grid (mapv vec grid)
     :mx mx
     :my my
     :edge (concat (for [x (range mx)] [x 0])
                   (for [x (range mx)] [x (dec my)])
                   (for [y (range my)] [0 y])
                   (for [y (range my)] [(dec mx) y]))
     :points (for [x (range mx)
                   y (range my)] [x y])}))

(def x1 (world [".#..#"
                "....."
                "#####"
                "....#"
                "...##"]))

(def x2 (world [".#....#####...#.."
                "##...##.#####..##"
                "##...#...#.#####."
                "..#.....#...###.."
                "..#.#.....#....##"]))

(defn read-world [n]
  (world (str/split-lines (slurp (io/resource n)))))

(def input (read-world "d10.txt"))

(defn gcd [a b]
  (int (.gcd (biginteger a) (biginteger b))))

(defn polar [[x y]]
  {:r (Math/sqrt (+ (* x x) (* y y)))
   :theta (Math/atan2 y x)})

(defn rot90 [[x y]] [(- y) x])

(defn rot270 [[x y]] [y (- x)])

(defn flip-y-axis [[x y]] [x (- y)])

(defn relative-polar
  "clockwise with north=0"
  [from to]
  (let [p (->> (map - to from)
               flip-y-axis
               rot270
               polar)]
    (if (> (:theta p) 0)
      p
      (update p :theta + (* 2 Math/PI)))))

(defn beam [from to]
  (let [[dx dy] (map - to from)
        d (gcd dx dy)
        dx (/ dx d)
        dy (/ dy d)]
    (concat (->> (iterate #(mapv + [dx dy] %) from)
                 (drop 1)
                 (take-while #(not= % to)))
            [to])))

(defn beams-from [from to-points]
  (->> to-points
       (remove #{from})
       (map #(beam from %))
       (partition-by first)
       (map (comp last (partial sort-by count)))))

(defn find-asteroids [world points]
  (filter #(= \# (get-in (:grid world) (reverse %))) points))

(defn visible-asteroids
  [world from]
  (let [beams (beams-from from (:points world))
        asteroids-on-beams (map #(find-asteroids world %) beams)
        hidden-asteroids (mapcat rest asteroids-on-beams)
        total-asteroids (find-asteroids world (:points world))]
    (->> total-asteroids
         (remove (set hidden-asteroids))
         (remove #{from})
         (sort-by (comp - :theta #(relative-polar from %))))))

(defn hot-spot [world]
  (->> (find-asteroids world (:points world))
       (map #(vector % (count (visible-asteroids world %))))
       (sort-by second)
       last))

(defn zap-asteroids [world points]
  (reduce (fn [world p]
            (update world :grid assoc-in (reverse p) \.))
          world
          points))

(defn rotate-laser [{:keys [world hits station] :as state}]
  (let [v (visible-asteroids world station)]
    ;;(println "zapping" v)
    (-> state
        (update :hits (fnil into []) v)
        (update :world zap-asteroids v))))

;; part 1
(hot-spot (read-world "d10a.txt"))
(hot-spot (read-world "d10b.txt")) ;; should be [5 8] 33
(hot-spot (read-world "d10c.txt")) ;; should be [1 2] 35
(hot-spot (read-world "d10d.txt")) ;; should be [6 3] 41
(hot-spot (read-world "d10e.txt")) ;; should be [11 13] 210
(hot-spot (read-world "d10.txt"))

(do
  (println "--")
  (->> {:world (read-world "d10e.txt") :station [11 13]}
       rotate-laser
       rotate-laser
       rotate-laser
       rotate-laser
       rotate-laser
       rotate-laser
       rotate-laser
       rotate-laser
       :hits
       (take 200)
       last
       ;;:world :grid (run! println)
       ))

(do
  (println "--")
  (->> {:world (read-world "d10.txt") :station [19 14]}
       rotate-laser
       rotate-laser
       :hits
       (take 200)
       last
       ;;:world :grid (run! println)
       ))

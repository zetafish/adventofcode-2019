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

(defn contour [{:keys [mx my]} [x y]]
  (let [start [x 0]
        north-west (reverse (map #(vector % 0) (range 0 x)))
        west (map #(vector 0 %) (range 1 my))
        south (map #(vector % (dec my)) (range 1 mx))
        east (reverse (map #(vector (dec mx) %) (range (dec my))))
        north-east (reverse (map #(vector % 0) (range (inc x) (dec mx))))]
    (remove #{[x y]}
            (concat [start] north-west west south east north-east))))

(defn beam [from to]
  (let [[dx dy] (map - to from)
        d (gcd dx dy)
        dx (/ dx d)
        dy (/ dy d)]
    (concat (->> (iterate #(mapv + [dx dy] %) from)
                 (drop 1)
                 (take-while #(not= % to)))
            [to])))

(defn beams-from [world from]
  (->> (contour world from)
       (map #(beam from %))
       (partition-by first)
       (map (comp last (partial sort-by count)))))

(defn find-asteroids [world points]
  (filter #(= \# (get-in (:grid world) (reverse %))) points))

(defn visible-asteroids
  [world from]
  (let [beams (beams-from world from)
        asteroids-on-beams (map #(find-asteroids world %) beams)
        hidden-asteroids (mapcat rest asteroids-on-beams)
        total-asteroids (find-asteroids world (:points world))]
    (->> total-asteroids
         (remove (set hidden-asteroids))
         (remove #{from}))))

(defn hot-spot [world]
  (->> (find-asteroids world (:points world))
       (map #(vector % (count (visible-asteroids world %))))
       (sort-by second)
       last))

;; part 1
(hot-spot (read-world "d10a.txt"))
(hot-spot (read-world "d10b.txt")) ;; should be [5 8] 33
(hot-spot (read-world "d10c.txt")) ;; should be [1 2] 35
(hot-spot (read-world "d10d.txt")) ;; should be [6 3] 41
(hot-spot (read-world "d10e.txt")) ;; should be [11 13] 210

(let [w (read-world "d10b.txt")]
  (map first (beams-from w [5 2])))


(->> (visible-asteroids (read-world "d10b.txt") [5 2])
     (map #(mapv - [5 2] %))
     sort)

(read-world "d10c.txt")

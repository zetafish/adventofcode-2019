(ns aoc.d6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def constellation
  (->> (str/split-lines (slurp (io/resource "d6.txt")))
       (map #(str/split % #"\)"))
       flatten
       (map (comp keyword str/lower-case))
       (partition 2)))

(def example
  [[:com :b]
   [:b :c]
   [:c :d]
   [:d :e]
   [:e :f]
   [:b :g]
   [:g :h]
   [:d :i]
   [:e :j]
   [:j :k]
   [:k :l]])

(def example2 (concat example [[:k :you] [:i :san]]))

(defn build-galaxy
  [rels]
  {:rels rels
   :objs (distinct (flatten rels))})

(defn inner->outer
  [galaxy x]
  (->> (:rels galaxy)
       (filter #(= x (first %)))
       (map second)))

(defn outer->inner
  [galaxy x]
  (->> (:rels galaxy)
       (filter #(= x (second %)))
       (map first)))

(defn roots
  [galaxy]
  (filter (complement (comp seq (partial outer->inner galaxy)))
          (:objs galaxy)))

(defn scan
  [objs f]
  (loop [objs objs d 0 cost {}]
    (if-not (seq objs)
      cost
      (let [ring (->> objs
                      (map f)
                      (flatten)
                      (distinct)
                      (remove cost))]
        (recur ring
               (inc d)
               (into cost (map #(vector % d)) objs))))))

(defn part1
  [orbits]
  (let [g (build-galaxy orbits)
        r (roots g)
        f (partial inner->outer g)]
    (reduce + (vals (scan r f)))))

;; (part1 example)
;; (part1 constellation)

(defn part2
  [orbits]
  (let [g (build-galaxy orbits)
        san-o (first (outer->inner g :san))
        you-o (first (outer->inner g :you))
        cost (scan [san-o]
                   (juxt (partial inner->outer g)
                         (partial outer->inner g)))]
    (get cost you-o)))


(part2 example2)
(part2 constellation)

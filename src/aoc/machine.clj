(ns aoc.machine
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-code [f]
  (->> (str/split (str/trim-newline (slurp (io/resource f))) #",")
       (map #(Integer/parseInt %))
       vec))

(keep-indexed (fn [i x] [i x]) [ 4 5 6])

(defn machine [code]
  {:memory (zipmap (iterate inc 0) code)
   :n 0
   :input []
   :output []
   :base 0})


(defn memory-at [machine n]
  (get-in machine [:memory n]))


(defmulti trace (fn [{:keys [n memory]}] (rem (get memory n) 100)))

(defmethod trace 1 [{:keys [n memory] :as machine}]
  (-> machine
      (update :n + 4)
      (assoc-in [:memory (memory (+ 3 n))]
                (+ (memory )))

      (assoc-in [:memory ] (+' (memory a) (memory b)))))

(defmethod step 2 [{memory :memory :as machine}]
  (-> machine
      (update :n + 4)
      (assoc-in [:memory c] (*' (memory a) (memory b)))))

(defmethod step 99 [machine]
  (assoc machine :halted true))

(defn seed-machine [machine [a b]]
  (-> machine
      (assoc-in [:memory 1] a)
      (assoc-in [:memory 2] b)))

(defn run [machine]
  (loop [machine machine]
    (if (:halted machine)
      machine
      (recur (step machine)))))

(-> (machine [1,9,10,3,2,3,11,0,99,30,40,50])
    step
    step
    )

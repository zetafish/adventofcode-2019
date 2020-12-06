(ns aoc.machine
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; puzzles that use the machine
(def puzzles [2])

(defn read-code [f]
  (->> (str/split (str/trim-newline (slurp (io/resource f))) #",")
       (map #(Integer/parseInt %))
       vec))

(defn machine [code]
  {:memory code :n 0})

(defn memory-at [machine n]
  (get-in machine [:memory n]))

(defmulti step (fn [{:keys [memory n]}] (get memory n)))

(defmethod step 1 [{memory :memory :as machine}]
  (-> machine
      (update :n + 4)
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

(ns aoc.d7
  (:require [aoc.core :as core]
            [clojure.math.combinatorics :refer [permutations]]))

(def x1 [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])

(def x2 [3 23 3 24 1002 24 10 24 1002 23 -1 23
          101 5 23 23 1 24 23 23 4 23 99 0 0])

(def x3 [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33
         1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0])

(def x4 [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26
         27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5])

(def x5 [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54
         -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4
         53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10])

(def code (core/read-code "d7.txt"))

(def machine (core/machine code))

(defn amp [machine phase]
  (update-in machine [:input] conj phase))

(defn circuit [machine phases]
  (map (partial amp machine) phases))

(defn single-pass
  [amps]
  (loop [input 0 amps amps]
    (if (empty? amps)
      input
      (let [amp (-> (first amps)
                    (update :input conj input)
                    core/run-to-output)
            input (last (:output amp))]
        (recur input (rest amps))))))

(defn run-feedback
  [amps]
  (loop [input 0 amps (vec amps) n 0]
    (if (every? :halted amps)
      input
      (let [amp (-> (get amps n)
                    (update :input conj input)
                    core/run-to-output)
            input (last (:output amp))]
        (recur input (assoc amps n amp) (rem (inc n) 5))))))


;; part 1
(->> (permutations [4 3 2 1 0])
     (map #(circuit machine %))
     (map single-pass)
     (apply max))

;; part 2
(->> (permutations [9 8 7 6 5])
     (map #(circuit machine %))
     (map run-feedback)
     (apply max))

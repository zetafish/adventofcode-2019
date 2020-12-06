(ns aoc.d2
  (:require [aoc.core :as core]))

(def machine (core/machine (core/read-code "d2.txt")))

(defn seed
  [machine [a b]]
  (-> machine
      (assoc-in [:memory 1] a)
      (assoc-in [:memory 2] b)))

;; part 1
(-> machine
    (seed [12 2])
    (core/run)
    (get-in [:memory 0]))

;; part 2
(for [a (range 100)
      b (range 100)
      :when (= 19690720 (-> machine
                            (seed [a b])
                            (core/run)
                            (get-in [:memory 0])))]
  (+ (* 100 a) b))

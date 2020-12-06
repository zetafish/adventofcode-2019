(ns aoc.d2
  (:require [aoc.core :as core]))

(def machine (core/machine (core/read-code "d2.txt")))

;; part 1
(-> machine
    (core/set-seed [12 2])
    (core/run)
    (get-in [:memory 0]))

;; part 2
(for [a (range 100)
      b (range 100)
      :when (= 19690720 (-> machine
                            (core/set-seed [a b])
                            (core/run)
                            (get-in [:memory 0])))]
  (+ (* 100 a) b))

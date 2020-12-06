(ns aoc.d5
  (:require [aoc.core :as core]))

(def code (core/read-code "d5.txt"))

(def machine (core/machine code))

;; part 1
(last (:output (core/run machine [1])))

;; part 2
(last (:output (core/run machine [5])))

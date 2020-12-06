(ns aoc.d9
  (:require [aoc.core :as core]))

(def code (core/read-code "d9.txt"))
(def mach (core/machine code))

(def x1 [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99])
(def x2 [1102 34915192 34915192 7 4 7 99 0])
(def x3 [104 1125899906842624 99])


(core/run (core/machine x1))
(core/run (core/machine x2))
(core/run (core/machine x3))

;; part 1
(core/run (assoc (core/machine code) :input [1]))

;; part 2
(core/run (assoc (core/machine code) :input [2]))

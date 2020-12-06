(ns aoc.d9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ac19.core :as core]))

(def raw (slurp (io/resource "d9.txt")))

(def code (->> (str/split (str/trim-newline raw) #",")
               (map #(Integer/parseInt %))
               vec))

(def x1 [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99])
(def x2 [1102 34915192 34915192 7 4 7 99 0])
(def x3 [104 1125899906842624 99])


(->> (core/machine code [1])
     (iterate core/step)
     (drop-while (complement :halted))
     first
     :memory)

(->> (core/machine code)
     core/step
     core/step
     core/step
     :memory
     )

(frequencies (map #(mod % 10) code))

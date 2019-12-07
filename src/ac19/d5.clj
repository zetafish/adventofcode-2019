(ns ac19.d5
  (:require [ac19.core :as core]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def raw (slurp (io/resource "d5.txt")))

(def code (->> (str/split (str/trim-newline raw) #",")
               (map #(Integer/parseInt %))
               vec))

;; part 1
(:output (core/run (core/machine code [1])))

;; part 2
(:output (core/run (core/machine code [5])))

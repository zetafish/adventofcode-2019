(ns ac19.d7
  (:require [ac19.core :as core]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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

(def raw (slurp (io/resource "d7.txt")))

(def code (->> (str/split (str/trim-newline raw) #",")
               (map #(Integer/parseInt %))
               vec))

(defn cart
  "Cartesian product"
  [colls]
  (if-not (seq colls)
    [[]]
    (for [more (cart (rest colls))
          x (first colls)]
      (cons x more))))

(defn phase-options
  "Returns valid phase options"
  [nums]
  (let [n (count nums)]
    (filter #(= n (count (distinct %)))
            (cart (repeat n nums)))))

(defn amp
  "Create an amp"
  [code phase]
  (core/machine code [phase]))

(defn amps
  "Create a circuit of amps"
  [code settings]
  (mapv (partial amp code) settings))

(defn halted?
  "Returns `true` if any of the amps is halted."
  [{:keys [amps]}]
  (seq (keep :halted amps)))

(defn produce-output
  [amp signal]
  (let [n (count (:output amp))]
    (->> (update amp :input conj signal)
         (iterate core/step)
         (drop-while #(and (not (:halted %))
                           (= n (count (:output %)))))
         first)))

(defn run
  "Sends `signal` to the first amp, use the output as the signal to the
  next amp etc. until the last one."
  [{:keys [amps signal]}]
  (let [a (produce-output (amps 0) signal)
        b (produce-output (amps 1) (first (:output a)))
        c (produce-output (amps 2) (first (:output b)))
        d (produce-output (amps 3) (first (:output c)))
        e (produce-output (amps 4) (first (:output d)))]
    {:amps [a b c d e]
     :signal (first (:output e))}))

(defn feedback
  "Run feedback loop until the amps are halted."
  [circuit]
  (->> (iterate run circuit)
       (drop-while (complement halted?))
       first))

(defn maximize
  [f nums code]
  (->> (phase-options nums)
       (map #(amps code %))
       (map #(hash-map :amps % :signal 0))
       (map f)
       (map :signal)
       (sort)
       (last)))

(run {:amps (amps x1 [4 3 2 1 0]) :signal 0})
(run {:amps (amps x2 [0 1 2 3 4]) :signal 0})
(run {:amps (amps x3 [1 0 4 3 2]) :signal 0})

;; part 1
(def maximize-run (partial maximize run [0 1 2 3 4]))
(maximize-run x1)
(maximize-run x1)
(maximize-run x3)
(maximize-run code)

;; part 2
(feedback (amps x4 [9 8 7 6 5]))
(feedback (amps x5 [9 7 8 5 6]))

(def maximize-feedback (partial maximize feedback [5 6 7 8 9]))
(maximize-feedback x4)
(maximize-feedback x5)
(maximize-feedback code)

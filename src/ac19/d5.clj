(ns ac19.d5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [ac19.core :as core]))

(def raw (slurp (io/resource "d5.txt")))

(def input (->> (str/split (str/trim-newline raw) #",")
                (map #(Integer/parseInt %))
                vec))

(defn parse-inst
  [n]
  {:opcode (rem n 100)
   :m1 (-> n (quot 100) (rem 10))
   :m2 (-> n (quot 1000) (rem 10))})

(defn get-value
  [v mode i]
  (case mode
    0 (v (v i))
    1 (v i)))


(defn step
  [{:keys [v n input] :as state}]
  (println n (take 4 v))
  (let [{:keys [opcode m1 m2]} (parse-inst (v n))]
    (case opcode
      1 (-> state
            (update :n + 4)
            (assoc-in [:v (v (+ 3 n))] (+ (get-value v m1 (+ 1 n))
                                          (get-value v m2 (+ 2 n)))))
      2 (-> state
            (update :n + 4)
            (assoc-in [:v (v (+ 3 n))] (* (get-value v m1 (+ 1 n))
                                          (get-value v m2 (+ 2 n)))))
      3 (-> state
            (update :n + 2)
            (assoc-in [:v (v (+ 1 n))] input))
      4 (-> state
            (update :n + 2)
            (assoc :halted true
                   :output (get-value v m1 (+ 1 n))))
      99 (assoc state :halted true))))

(-> {:v input :n 0 :input 1}
    step
    step
    step
    step
    step
    step
    step
    )

(->> {:v input :n 0 :input 1}
     (iterate step)
     (drop-while (complement :halted))
     first
     :output)

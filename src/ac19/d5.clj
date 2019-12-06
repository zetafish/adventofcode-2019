(ns ac19.d5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [ac19.core :as core]))

(def raw (slurp (io/resource "d5.txt")))

(def codes (->> (str/split (str/trim-newline raw) #",")
                (map #(Integer/parseInt %))
                vec))

(defn parse-inst
  [n]
  {:opcode (rem n 100)
   :m1 (-> n (quot 100) (rem 10))
   :m2 (-> n (quot 1000) (rem 10))
   :m3 (-> n (quot 10000) (rem 10))

   ;; :m3 ...
   })

(defn get-value
  [v mode i]
  (case mode
    0 (v (v i))
    1 (v i)))

(defn step
  [{:keys [v n input] :as state}]
  (println (str "n=" n ", v=" (vec (take 5 (drop n v)))))
  (let [{:keys [opcode m1 m2 m3]} (parse-inst (v n))]
    (case opcode
      ;; ADD
      1 (let [a (get-value v m1 (+ 1 n))
              b (get-value v m2 (+ 2 n))
              c (v (+ 3 n))]
          (-> state
              (update :n + 4)
              (assoc-in [:v c] (+ a b))))

      ;; MUL
      2 (let [a (get-value v m1 (+ 1 n))
              b (get-value v m2 (+ 2 n))
              c (v (+ 3 n))]
          (-> state
              (update :n + 4)
              (assoc-in [:v c] (* a b))))

      ;; RECV
      3 (let [c (v (+ 1 n))]
          (-> state
              (update :n + 2)
              (assoc-in [:v c] input)))

      ;; SEND
      4 (let [a (get-value v m1 (+ 1 n))]
          (-> state
              (update :n + 2)
              (update :output conj a)))

      ;; JNZ
      5 (let [a (get-value v m1 (+ 1 n))
              b (get-value v m2 (+ 2 n))]
          (if-not (zero? a)
            (assoc state :n b)
            (update state :n + 3)))

      ;; JZ
      6 (let [a (get-value v m1 (+ 1 n))
              b (get-value v m2 (+ 2 n))]
          (if (zero? a)
            (assoc state :n b)
            (update state :n + 3)))

      ;; LT
      7 (let [a (get-value v m1 (+ 1 n))
              b (get-value v m2 (+ 2 n))
              c (v (+ 3 n))]
          (-> state
              (update :n + 4)
              (assoc-in [:v c] (if (< a b) 1 0))))

      ;; EQ
      8 (let [a (get-value v m1 (+ 1 n))
              b (get-value v m2 (+ 2 n))
              c (v (+ 3 n))]
          (-> state
              (update :n + 4)
              (assoc-in [:v c] (if (= a b) 1 0))))

      99 (assoc state :halted true))))

(defn run-program
  [codes input]
  (->> {:v codes :input input :output [] :n 0 }
       (iterate step)
       (drop-while (complement :halted))
       first))

;; part 1
(:output (run-program codes 1))

;; part 2
(:output (run-program codes 5))

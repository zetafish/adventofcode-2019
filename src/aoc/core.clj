(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Related puzzles
;; 2: ADD(1),MUL(2),HALT(99)
;; 5: RECV(3),EMIT(4)

(defn digits [n]
  (loop [v nil n n]
    (if (zero? n)
      (if (seq v) v [0])
      (recur (cons (rem n 10) v) (quot n 10)))))

(defn parse-inst
  [n]
  {:opcode (rem n 100)
   :m [(-> n (quot 100) (rem 10))
       (-> n (quot 1000) (rem 10))
       (-> n (quot 10000) (rem 10))]})

(defn read-code
  [f]
  (->> (str/split (str/trim-newline (slurp (io/resource f))) #",")
               (map #(Integer/parseInt %))
               vec))

(def mnemonic
  {1 ["ADD" 3]
   2 ["MUL" 3]
   3 ["RECV" 1]
   4 ["EMIT" 1]
   5 ["JNZ" 2]
   6 ["JZ" 2]
   7 ["LT" 3]
   8 ["EQ" 3]
   9 ["SETBP" 1]
   99 ["HALT"]})

(defn step
  [{:keys [memory base n input] :as state}]
  (let [fetch (fn [i] (get memory i 0))
        {:keys [opcode m]} (parse-inst (fetch n))
        par (fn [i]
              (case (m (dec i))
                0 (fetch (fetch (+ i n)))
                1 (fetch (+ i n))
                2 (fetch (+ base (fetch (+ i n))))))]
    (condp =  opcode
      ;; ADD
      1 (-> state
            (update :n + 4)
            (assoc-in [:memory (memory (+ 3 n))] (+' (par 1) (par 2))))
      ;; MUL
      2 (-> state
            (update :n + 4)
            (assoc-in [:memory (memory (+ 3 n))] (*' (par 1) (par 2))))
      ;; RECV
      3 (-> state
            (update :n + 2)
            (assoc-in [:memory (memory (+ 1 n))] (first input))
            (update :input rest))
      ;; EMIT
      4 (-> state
            (update :n + 2)
            (update :output conj (par 1)))
      ;; JNZ
      5 (if-not (zero? (par 1))
          (assoc state :n (par 2))
          (update state :n + 3))
      ;; JZ
      6 (if (zero? (par 1))
          (assoc state :n (par 2))
          (update state :n + 3))
      ;; LT
      7 (-> state
            (update :n + 4)
            (assoc-in [:memory (memory (+ 3 n))]
                      (if (< (par 1) (par 2)) 1 0)))
      ;; EQ
      8 (-> state
            (update :n + 4)
            (assoc-in [:memory (memory (+ 3 n))]
                      (if (= (par 1) (par 2)) 1 0)))
      ;; SETBP
      9 (-> state
            (update :n + 2)
            (update :base + (par 1)))
      ;; HALT
      99 (assoc state :halted true)
      #_(throw (ex-info "Bad case"
                        {:state state
                         :pi (parse-inst (memory n))
                         :opcode opcode})))))

(defn disassemble
  [{:keys [memory n] :as state}]
  (let [{:keys [opmemory m]} (parse-inst (memory n))
        param (fn [i]
                (let [v (memory (+ i 1 n))]
                  (if (= 0 (m i)) [v] v)))
        f (fn [mnemonic k]
            (concat [mnemonic] (map param (range k))))
        instr (case opmemory
                1 (f "ADD" 3)
                2 (f "MUL" 3)
                3 (f "RECV" 1)
                4 (f "EMIT" 1)
                5 (f "JNZ" 3)
                6 (f "JZ" 3)
                7 (f "LT" 3)
                8 (f "EQ" 3)
                9 (f "SETBP" 1)
                99 (f "HALT" 0)
                (f "NOP" 0))]
    (-> state
        (update :n + (count instr))
        (update :dis (fnil conj []) instr))))

(defn machine
  "Create a machine"
  [memory]
  {:memory (zipmap (iterate inc 0) memory)
   :cost 0
   :n 0
   :base 0
   :input []
   :output []})

(defn set-seed
  [machine [a b]]
  (-> machine
      (assoc-in [:memory 1] a)
      (assoc-in [:memory 2] b)))

(defn run
  "Run to HALT state"
  ([machine] (run machine []))
  ([machine input]
   (loop [machine (assoc machine :input input)]
     (if (:halted machine)
       machine
       (recur (step machine))))))

(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Related puzzles
;; 2: ADD(1),MUL(2),HALT(99)
;; 5: RECV(3),EMIT(4),JNZ(5),JZ(6),LT(7),EQ(8), modes: position(0),immediate(1)
;; 7: functional composition
;; 9: SETBP(9),  modes: relative(2)

(defn parse-inst
  "ABCDE
    1001

  DE - two digit opcode, 02 = opcode 2
   C - mode of 1st parameter, 0 = position mode
   B - mode of 2nd parameter, 1 = immediate mode
   A - mode of 3rd parameter, 0 = position mode,ommited due to leading zero"
  [n]
  {:opcode (rem n 100)
   :mode [(-> n (quot 100) (rem 10))
          (-> n (quot 1000) (rem 10))
          (-> n (quot 10000) (rem 10))]})

(defn read-code
  [f]
  (->> (str/split (str/trim-newline (slurp (io/resource f))) #",")
       (map #(Long/parseLong %))
       vec))

(defn step
  [{:keys [memory base n input] :as state}]
  (let [fetch (fn [k]
                (memory k 0))
        {:keys [opcode mode]} (parse-inst (fetch n))
        par (fn [i]
              (let [v (fetch (+ i n))]
                (case (mode (dec i))
                  0 (fetch (fetch (+ i n)))
                  1 (fetch (+ i n))
                  2 (fetch (+ base (fetch (+ i n)))))))
        modifier (fn [i]
                   (case (mode (dec i))
                     0 fetch
                     1 identity
                     2 (comp fetch (partial + base))))
        param (fn [i] (memory (+ i n)))
        param* (fn [i] ((modifier i) (param i)))
        loc (fn [i]
              (case (mode (dec i))
                0 (memory (+ i n))
                2 (+ base (memory (+ i n)))))]

    (condp = opcode
      ;; ADD
      1 (-> state
            (update :n + 4)
            (assoc-in [:memory (loc 3)] (+ (par 1) (par 2))))
      ;; MUL
      2 (-> state
            (update :n + 4)
            (assoc-in [:memory (loc 3)] (* (par 1) (par 2))))
      ;; RECV
      3 (-> state
            (update :n + 2)
            (assoc-in [:memory (loc 1)] (first input))
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
            (assoc-in [:memory (loc 3)]
                      (if (< (par 1) (par 2)) 1 0)))
      ;; EQ
      8 (-> state
            (update :n + 4)
            (assoc-in [:memory (loc 3)]
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

(defn machine
  "Create a machine"
  [memory]
  {:memory (zipmap (iterate inc 0) memory)
   :cost 0
   :n 0
   :base 0
   :input []
   :output []
   :fetches []})

(defn run
  "Run to HALT state"
  [machine]
  (loop [machine machine]
    (if (:halted machine)
      machine
      (recur (step machine)))))

(defn run-to-output
  [machine]
  (let [out (machine :output)]
    (loop [machine machine]
      (cond
        (:halted machine) machine
        (not= out (:output machine)) machine
        :else (recur (step machine))))))

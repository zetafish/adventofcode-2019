(ns ac19.core)

(defn digits [n]
  (loop [v nil n n]
    (if (zero? n)
      (if (seq v) v [0])
      (recur (cons (rem n 10) v) (quot n 10)))))

(defn parse-inst
  [n]
  (let [m [(-> n (quot 100) (rem 10))
           (-> n (quot 1000) (rem 10))
           (-> n (quot 10000) (rem 10))]]
    {:opmemory (rem n 100)
     :m m
     :m1 (m 0)
     :m2 (m 1)
     :m3 (m 2)}))

(defn step
  [{:keys [memory n input] :as state}]
  (let [{:keys [opmemory m]} (parse-inst (memory n))
        par (fn [i]
              (let [x (memory (+ i n))]
                (if (= 0 (m (dec i)))
                  (memory x)
                  x)))]
    (case opmemory
      ;; ADD
      1 (-> state
            (update :n + 4)
            (assoc-in [:memory (memory (+ 3 n))] (+ (par 1) (par 2))))
      ;; MUL
      2 (-> state
            (update :n + 4)
            (assoc-in [:memory (memory (+ 3 n))] (* (par 1) (par 2))))
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
      ;; HALT
      99 (assoc state :halted true))))

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
                99 (f "HALT" 0)
                (f "NOP" 0))]
    (-> state
        (update :n + (count instr))
        (update :dis (fnil conj []) instr))))

(defn machine
  "Create a machine"
  ([memory input]
   {:memory memory :n 0 :input input})
  ([memory]
   {:memory memory :n 0}))

(defn run
  "Run to HALT state"
  [machine]
  (loop [state machine]
    (if (:halted state)
      state
      (recur (step state)))))

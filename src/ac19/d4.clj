(ns d4)

(def start 138307)
(def end 654504)

(defn digits
  [n]
  (loop [d nil n n]
    (if (zero? n)
      (if-not (seq d)
        [0]
        d)
      (recur (conj d (mod n 10))
             (quot n 10)))))

(defn non-decreasing?
  [col]
  (apply <= col))

(defn has-pair?
  [col]
  (->>
   (partition-by identity col)
   (map count)
   (filter #(<= 2 %))
   seq))

(defn has-true-pair?
  [col]
  (->>
   (partition-by identity col)
   (map count)
   (filter #(= 2 %))
   seq))

(defn scan-range
  [a b pred]
  (->>
   (range a (inc b))
   (map digits)
   (filter pred)
   count))

;; part 1
(scan-range start end (every-pred non-decreasing? has-pair?))

;; part 2
(scan-range start end (every-pred non-decreasing? has-true-pair?))

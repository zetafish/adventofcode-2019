(ns ac19.core)

(defn digits [n]
  (loop [v nil n n]
    (if (zero? n)
      (if (seq v) v [0])
      (recur (cons (rem n 10) v) (quot n 10)))))

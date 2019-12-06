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

(defn has-n-tuple?
  [n col]
  (->>
   (map #(drop % col) (range n))
   (mapcat #(partition n %))
   (some (partial apply =))))

(defn non-decreasing?
  [col]
  (apply <= col))

(defn solve1 [a b]
  (->> (range a (inc b))
       (map digits)
       (filter (partial has-n-tuple? 2))
       (filter non-decreasing?)
       (count)))

(defn solve2 [a b]
  (->> (range a (inc b))
       (map digits)
       (filter (partial has-n-tuple? 2))
       (remove (partial has-n-tuple? 3))
       (filter non-decreasing?)
       (count)))

(solve1 start end)
(solve2 start end)

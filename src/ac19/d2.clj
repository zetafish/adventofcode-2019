(ns d2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (str/split (str/trim-newline (slurp (io/resource "d2.txt")))
                           #",")
                (map #(Integer/parseInt %))
                vec))

(defn run-prog
  [input noun verb]
  (loop [n 0 v (assoc input
                      1 noun
                      2 verb)]
    (case (v n)
      1 (recur (+ 4 n) (assoc v (v (+ 3 n))
                              (+ (v (v (+ 1 n))) (v (v (+ 2 n))))))
      2 (recur (+ 4 n) (assoc v (v (+ 3 n))
                              (* (v (v (+ 1 n))) (v (v (+ 2 n))))))
      99 (first v))))

(defn find-noun-verb
  [input output]
  (first (remove nil? (for [noun (range 100)
                            verb (range 100)]
                        (let [v (run-prog input noun verb)]
                          (when (= v output)
                            (+ (* 100 noun) verb)))))))

;; part 1
(run-prog input 12 2)

;; part 2
(find-noun-verb input 19690720)

(ns advent2017.day6
  (:require [net.cgrand.xforms :as x]
    [clojure.edn :as edn]))

(defn part1 [input]
  (loop [banks (edn/read-string (str "[" input "]")) seen? #{} n 1]
    (let [seen? (conj seen? banks)
          selected-bank (apply max-key banks (range (dec (count banks)) -1 -1))
          cells (nth banks selected-bank)
          banks'
          (reduce
            (fn [banks i]
              (update banks (mod i (count banks)) inc))
            (assoc banks selected-bank 0)
            (range (inc selected-bank) (+ (inc selected-bank) cells)))]
      (if (seen? banks')
        n
        (recur banks' seen? (inc n))))))

(defn part2 [input]
  (loop [banks (edn/read-string (str "[" input "]")) prev-index {} n 1]
    (let [prev-index (assoc prev-index banks n)
          selected-bank (apply max-key banks (range (dec (count banks)) -1 -1))
          cells (nth banks selected-bank)
          banks'
          (reduce
            (fn [banks i]
              (update banks (mod i (count banks)) inc))
            (assoc banks selected-bank 0)
            (range (inc selected-bank) (+ (inc selected-bank) cells)))]
      (if-some [i (prev-index banks')]
        (- n i)
        (recur banks' prev-index (inc n))))))
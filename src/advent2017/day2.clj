(ns advent2017.day2
  (:require [net.cgrand.xforms :as x]))

(defn part1 [input]
  (transduce
    (mapcat
      (fn [line]
        (x/some (comp (map #(Long/parseLong %))
                  (x/transjuxt [x/max (comp x/min (map -))])) (re-seq #"\d+" line))))
    + (re-seq #"[^\n]+" input)))

(defn part2 [input]
  (transduce
    (x/for [line %
            :let [nums (into [] (map #(Long/parseLong %)) (re-seq #"\d+" line))]
            i nums
            j nums
            :when (and (> i j) (zero? (mod i j)))]
      (quot i j))
    + (re-seq #"[^\n]+" input)))
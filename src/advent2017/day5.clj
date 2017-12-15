(ns advent2017.day5
  (:require [net.cgrand.xforms :as x]
    [clojure.edn :as edn]))

(defn part1 [input]
  (loop [pc 0 pgm (edn/read-string (str "[" input "]")) n 0]
    (if (< -1 pc (count pgm))
      (recur (+ pc (nth pgm pc)) (update pgm pc inc) (inc n))
      n)))

(defn part2 [input]
  (loop [pc 0 pgm (edn/read-string (str "[" input "]")) n 0]
    (if (< -1 pc (count pgm))
      (recur (+ pc (nth pgm pc)) (update pgm pc (if (<= 3 (nth pgm pc)) dec inc)) (inc n))
      n)))
(ns advent2017.day11
  (:require [net.cgrand.xforms :as x]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))

(defn part1 [input]
  (let [[x y]
        (reduce (fn [[x y] [ns ew]]
                  [(+ x ({\e 1 \w -1} ew 0)) (({\n + \s -} ns) y ({nil 2} ew 1))])
          [0 0]
          (re-seq #"\w+" input))
        x (Math/abs x)
        y (Math/abs y)]
    (if (< x y)
      (+ x (quot (- y x) 2))
      x)))

(defn part2 [input]
  (let [d (fn [[x y]]
            (let [x (Math/abs x)
                  y (Math/abs y)]
              (if (< x y)
                (+ x (quot (- y x) 2))
                x)))]
    (x/some
      (comp
        (x/reductions (fn [[x y] [ns ew]]
                        [(+ x ({\e 1 \w -1} ew 0)) (({\n + \s -} ns) y ({nil 2} ew 1))])
          [0 0])
        (map d)
        x/max)
      (re-seq #"\w+" input))))
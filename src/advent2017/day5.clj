(ns advent2017.day4
  (:require [net.cgrand.xforms :as x]))

(defn part1 [input]
  (x/count 
    (remove (fn [passphrase]
              (->> passphrase (re-seq #"[a-z]+") frequencies vals (some #(> % 1)))))
    (re-seq #"[a-z ]+" input)))

(defn part2 [input]
  (x/count 
    (remove (fn [passphrase]
              (->> passphrase (re-seq #"[a-z]+") (map sort) frequencies vals (some #(> % 1)))))
    (re-seq #"[a-z ]+" input)))
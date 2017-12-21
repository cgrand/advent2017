(ns advent2017.day10
  (:require [net.cgrand.xforms :as x]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))

(defn part1 [input]
  (let [N 256
        [circle _ pos] (reduce 
                         (fn [[circle skip pos] length]
                           [(->> (concat (reverse (take length circle)) (drop length circle))
                              cycle
                              (drop (+ length skip))
                              (take N)) (inc skip) (+ pos length skip)])
                         [(range N) 0 0]
                         (edn/read-string (str "[" input "]")))
        circle (concat (drop (mod (- pos) N) circle) (take (mod (- pos) N) circle))]
    (apply * (take 2 circle))))

(defn part2 [input]
  (let [N 256
        input (apply concat (repeat 64 (concat (map long input) [17, 31, 73, 47, 23])))
        [circle _ pos] (reduce 
                         (fn [[circle skip pos] length]
                           [(->> (concat (reverse (take length circle)) (drop length circle))
                              cycle
                              (drop (+ length skip))
                              (take N)) (inc skip) (+ pos length skip)])
                         [(range N) 0 0]
                         input)
        circle (concat (drop (mod (- pos) N) circle) (take (mod (- pos) N) circle))
        dense (x/into [] (x/partition 16 (x/reduce (fn ([] 0) ([x] x) ([a b] (bit-xor a b))))) circle)]
    (apply format (apply str (repeat 16 "%02x")) dense)))
(ns advent2017.day15
  (:require
    [net.cgrand.xforms :as x]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))

(defn gen-step [factor]
  (fn [prev] (rem (* prev factor) 2147483647)))

(defn part1 [start-a start-b]
  (let [a (take 4e7 (next (iterate (gen-step 16807) start-a)))
        b (next (iterate (gen-step 48271) start-b))]
    (x/count (filter identity) (map (fn [a b] (zero? (bit-and 0xffff (bit-xor a b)))) a b))))

(defn part2 [start-a start-b]
  (let [a (take 5e6 (filter #(zero? (bit-and % 3)) (next (iterate (gen-step 16807) start-a))))
        b (filter #(zero? (bit-and % 7)) (next (iterate (gen-step 48271) start-b)))]
    (x/count (filter identity) (map (fn [a b] (zero? (bit-and 0xffff (bit-xor a b)))) a b))))

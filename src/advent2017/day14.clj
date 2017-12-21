(ns advent2017.day14
  (:use [advent2017.day10 :only [part2] :rename {part2 knothash}])
  (:require
    [net.cgrand.xforms :as x]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))

(def bitcount
  (into {} (map (fn [n] [(first (format "%x" n)) (Long/bitCount n)])) (range 16)))

(defn part1 [input]
  (let [N 128]
    (transduce
      (comp
        (mapcat #(knothash (str input "-" %)))
        (map bitcount))
      + 0 (range N))))

(def bitsets
  (into {}
    (map (fn [n] [(first (format "%x" n))
                  (into #{} (keep-indexed (fn [i c] (case c \1 i nil))) (reverse (Long/toBinaryString n)))]))
    (range 16)))

(defn part2 [input]
  (let [N 128
        grid (into #{}
               (x/for [i %
                       :let [bitsets (into [] (map bitsets) (reverse (knothash (str input "-" i))))]
                       j (range 0 N 4)
                       dj (nth bitsets (quot j 4))]
                 [i (+ j dj)])
               (range N))
        group (fn [x]
                (loop [members #{x} todo #{x}]
                  (if-some [[i j] (first todo)]
                    (let [new (filter #(and (grid %) (not (members %))) [[(inc i) j] [i (inc j)] [(dec i) j] [i (dec j)]])]
                      (recur (into members new) (-> todo (into new) (disj [i j]))))
                    members)))]
    (loop [n 0 grid grid]
      (if-some [x (first grid)]
        (recur (inc n) (reduce disj grid (group x)))
        n))))
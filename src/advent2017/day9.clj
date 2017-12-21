(ns advent2017.day9
  (:require [net.cgrand.xforms :as x]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))

(defn part1 [input]
  (first
    (reduce (fn [[total-score score state] c]
              (case [state c]
                [:normal \}] [(+ score total-score) (dec score) state]
                [:normal \{] [total-score (inc score) state]
                [:normal \<] [total-score score :garbage]
                [:garbage \>] [total-score score :normal]
                [:garbage \!] [total-score score :garbage-esc]
                [total-score score ({:garbage-esc :garbage} state state)]))
      [0 0 :normal]
      input)))

(defn part2 [input]
  (first
    (reduce (fn [[n state] c]
              (case [state c]
                [:normal \}] [n state]
                [:normal \{] [n state]
                [:normal \<] [n :garbage]
                [:garbage \>] [n :normal]
                [:garbage \!] [n :garbage-esc]
                [(+ n ({:garbage 1} state 0)) ({:garbage-esc :garbage} state state)]))
      [0 :normal]
      input)))
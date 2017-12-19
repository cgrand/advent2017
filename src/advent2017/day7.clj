(ns advent2017.day7
  (:require [net.cgrand.xforms :as x]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))

(defn part1 [input]
  (let [input (s/conform (s/* (s/cat :name symbol? :weight (s/and (s/cat :weight int?) (s/conformer #(:weight %)))
                                :children (s/? (s/& (s/cat :_ #{'->} :children (s/* symbol?)) (s/conformer #(:children %))))))
                (edn/read-string (str "[" input "]")))
        [parents children] (x/transjuxt [(comp (map :name) (x/into #{})) (comp (mapcat :children) (x/into #{}))] input)]
    (first (reduce disj parents children))))

(defn part2 [input]
  (let [input (s/conform (s/* (s/cat :name symbol? :weight (s/and (s/cat :weight int?) (s/conformer #(:weight %)))
                                :children (s/? (s/& (s/cat :_ #{'->} :children (s/* symbol?)) (s/conformer #(:children %))))))
                (edn/read-string (str "[" input "]")))
        unbalanced
        (loop [nodes {}]
          (let [xform (x/for [{:keys [name weight children]} %
                             :when (not (nodes name))
                             :when (every? nodes children)
                             :let [children (into [] (map nodes) children)]]
                        [name (or (some #(when-not (:balanced %) %) children)
                                {:name name
                                 :own-weight weight
                                 :children children
                                 :weight (transduce (map :weight) + weight children)
                                 ; next line wouldn't hold if more than one was out of balance
                                 :balanced (< (count (into #{} (map :weight) children)) 2)})])]
            (if (= (count nodes) (dec (count input)))
             ; root!
             (:children (second (x/some xform input)))
             (recur (x/into nodes xform input)))))
        groups (group-by :weight unbalanced)
        expected-weight (some (fn [[w nodes]] (when (next nodes) w)) groups)
        outlier (some (fn [[w [node & nodes]]] (when-not nodes node)) groups)]
    (+ (:own-weight outlier) (- expected-weight (:weight outlier)))))
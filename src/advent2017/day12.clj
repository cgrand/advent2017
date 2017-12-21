(ns advent2017.day12
  (:require [net.cgrand.xforms :as x]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))

(defn part1 [input]
  (let [input (s/conform
                (s/* (s/cat :k int? :_ #{'<->} :vs (s/* int?)))
                (edn/read-string (str "[" input "]")))
        m (into {} (map (fn [{:keys [k vs]}] [k (set vs)])) input)
        reachable (fn [id]
                    (loop [members #{id} todo #{id}]
                      (if-some [id (first todo)]
                        (let [new (into #{} (remove members) (m id))]
                          (recur (into members new) (-> todo (disj id) (into new))))
                        members)))]
    (count (reachable 0))))

(defn part2 [input]
  (let [input (s/conform
                (s/* (s/cat :k int? :_ #{'<->} :vs (s/* int?)))
                (edn/read-string (str "[" input "]")))
        m (into {} (map (fn [{:keys [k vs]}] [k (set vs)])) input)
        reachable (fn [id]
                    (loop [members #{id} todo #{id}]
                      (if-some [id (first todo)]
                        (let [new (into #{} (remove members) (m id))]
                          (recur (into members new) (-> todo (disj id) (into new))))
                        members)))]
    (loop [n 0 m m]
      (if-some [id (key (first m))]
        (recur (inc n) (reduce dissoc m (reachable id)))
        n))))
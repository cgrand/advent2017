(ns advent2017.day16
  (:require
    [net.cgrand.xforms :as x]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))

(defn part1 [input]
  (reduce
    (fn [s [_ move x y]]
      (case move
        "s" (let [i (- (count s) (Long/parseLong x))]
              (str (subs s i) (subs s 0 i)))
        "x" (let [[i j] (sort [(Long/parseLong x) (Long/parseLong y)])]
              (str (subs s 0 i) (nth s j) (subs s (inc i) j) (nth s i) (subs s (inc j))))
        "p" (apply str (map #({(first x) y (first y) x} % %) s))))
   "abcdefghijklmnop"
   (re-seq #"([sxp])([0-9a-p]+)(?:/([0-9a-p]+))?" input)))

(defn part2 [input]
  (let [N 16
        a (long \a)
        s (apply str (map char (range a (+ a N))))
        input (re-seq #"([sxp])([0-9a-p]+)(?:/([0-9a-p]+))?" input)
        permutation
        (reduce
          (fn [v [_ move x y]]
            (case move
              "s" (let [i (- N (Long/parseLong x))]
                    (into (subvec v i) (subvec v 0 i)))
              "x" (let [[i j] (sort [(Long/parseLong x) (Long/parseLong y)])]
                    (-> (subvec v 0 i) (conj (nth v j)) (into (subvec v (inc i) j)) (conj (nth v i)) (into (subvec v (inc j)))))
              "p" v))
          (vec (range N)) input)
        renames
        (reduce
          (fn [m [_ move [x] [y]]]
            (case move
              ("s" "x") m
              "p" (assoc m x (m y) y (m x))))
          (zipmap s s) input)
        renames (zipmap (vals renames) (keys renames))
        pow (fn [f n] (memoize (apply comp (repeat n f))))
        p1e9 (comp #(pow % 1000) #(pow % 1000) #(pow % 1000))]
    (apply str (map (comp (p1e9 renames) #(char (+ a %)) (p1e9 permutation)) (range N)))))

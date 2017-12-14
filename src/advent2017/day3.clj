(ns advent2017.day3
  (:require [net.cgrand.xforms :as x]))

(defn part1 [input]
  (let [w (bit-or 1 (long (Math/ceil (Math/sqrt input))))
        start (* (- w 2) (- w 2))]
    (+ (quot w 2)
      (Math/abs (- (mod (- input start) (dec w)) (quot w 2))))))

(defn part2 [input]
  (let [w (bit-or 1 (long (Math/ceil (Math/sqrt input))))
        c (inc (quot w 2))
        offset (fn [[x y]] (+ x (* y w)))
        neighbours (into [] (x/for [i [-1 0 1] j [-1 0 1] :when (not= 0 i j)] (offset [i j])))
        ccw-dirs (map offset [[0 1] [-1 0] [0 -1] [1 0]])
        turn-left (zipmap ccw-dirs (next (cycle ccw-dirs)))]
    (loop [v (-> (into [] (repeat (* w w) 0)) (assoc (offset [c c]) 1)) p (offset [(inc c) c]) dir (first ccw-dirs)]
      (let [value (transduce (map #(nth v (+ p %))) + neighbours)
            v (assoc v p value)
            dir (if (zero? (nth v (+ p (turn-left dir)))) (turn-left dir) dir)]
        (if (> value input)
          value
          (recur v (+ p dir) dir))))))
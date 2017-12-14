(ns advent2017.day13)

(defn part1 [input]
  (transduce
    (comp (map #(Long/parseLong %)) (x/partition 2)
      (keep (fn [[depth range]] (when (zero? (mod depth (- (* 2 range) 2))) (* depth range)))))
    + (re-seq #"\d+" input)))

(defn part2 [input]
  (let [input (into []
                (comp (map #(Long/parseLong %)) (x/partition 2))
                (re-seq #"\d+" input))]
    (some (fn [n]
            (when (every? (fn [[depth range]] (pos? (mod (+ n depth) (- (* 2 range) 2)))) input)
              n))
      (range))))
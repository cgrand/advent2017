(ns advent2017.day8
  (:require [net.cgrand.xforms :as x]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))

(defn part1 [input]
  (let [input
        (s/conform (s/* (s/cat :reg symbol? :op (s/conformer #({'inc + 'dec -} % ::s/invalid)) :imm int? :_ #{'if}
                          :regc symbol? :opc (s/conformer #({'<= <= '>= >= '< < '> > '== = '!= not=} % ::s/invalid)) :immc int?))
          (edn/read-string (str "[" input "]")))
        regs (reduce 
               (fn [regs {:keys [reg op imm regc opc immc]}]
                 (cond-> regs
                   (opc (regs regc 0) immc) (update reg (fnil op 0) imm)))
               {} input)]
    (reduce max (vals regs))))

(defn part2 [input]
  (let [input
        (s/conform (s/* (s/cat :reg symbol? :op (s/conformer #({'inc + 'dec -} % ::s/invalid)) :imm int? :_ #{'if}
                          :regc symbol? :opc (s/conformer #({'<= <= '>= >= '< < '> > '== = '!= not=} % ::s/invalid)) :immc int?))
          (edn/read-string (str "[" input "]")))]
    (x/some (comp (x/reductions (fn [regs {:keys [reg op imm regc opc immc]}]
                                  (cond-> regs
                                    (opc (regs regc 0) immc) (update reg (fnil op 0) imm)))
                    {}) cat x/vals x/max) input)))
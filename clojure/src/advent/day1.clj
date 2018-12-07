(ns advent.day1
  (:require [clojure.string :as str]))


(defn parse
  [s]
  (mapv #(Long/parseLong %)
    (str/split-lines s)))

(def data (parse (slurp "../AOC1A.txt")))



; part 1
(apply + data)

(reduce (fn [a v] (let [ [h & t] a]   (cons (+ v h) a ))) [0] data)

(loop [ instrs data
        seen #{}
        acc 0]
  (if (empty? instrs)
    (recur data seen acc)
    (if (contains? seen acc)
      acc
      (recur  (next instrs)
              (conj seen acc)
              (+ acc (first instrs))))))


(defn redFn  [{:keys [seen acc]} instr]
  (let [v (+ acc instr)]
    (if (contains? seen v)
      (reduced v)
      { :seen (conj seen v)
        :acc v})))

(reduce redFn
  { :seen #{0}
    :acc 0}
  (cycle data))

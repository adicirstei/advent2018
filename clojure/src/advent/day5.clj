(ns advent.day5
  (:require
   [clojure.string :as str]))

(def data (slurp "../AOC5.txt"))
(def toy-data "dabAcCaCBAcCcaDA")


(def remove-chars "abcdefghijklmnopqrstuvwxyz")

(defn without-type
    [s t]
    (str/replace s t ""))

(without-type "absyjhdkhkshdkAAAAddadaA" #"[aA]")



(defn invert [ch]
  (if (Character/isLowerCase ch)
    (Character/toUpperCase ch)
    (Character/toLowerCase ch)))





(defn destroy-one
  [s]
  (loop [i 1]
    (if (>= i (count s) )
      s
      (if (= (nth s (dec i)) (invert (nth s i)))
        (str (subs s 0 (dec i)) (subs s (inc i)))
        (recur (inc i))))))




(loop [s data]
            (let [s' (destroy-one s)
                  c (count s')]
              (println c)
              (if (identical? s s')
                [c s']
                (recur s'))))


(map #(hash-map %   (count (reduce add-unit [] (filterv (fn [ch] (and (not= ch \newline ) (not= ch %) (not= (invert ch) %))   ) data)     ))   ) remove-chars)

(hash-map \a 42)


(defn reacts? [x y]
  (and (not= x y)
       (= (str/lower-case x) (str/lower-case y))))




(defn add-unit
  [polymer unit]
  (let [l (last polymer)]
    (if (and l (reacts? l unit))
      (pop polymer)
      (conj polymer unit))))




(ns advent.day2
  (:require
    [common :as common]
    [clojure.string :as str]
    [clojure.core.matrix :as matrix]))




(def data (slurp "../AOC2.txt"))

(defn parse
  "docstring"
  [s]
  (str/split-lines s))



(def parsed (parse data))

(defn score-token
  [token]
  (let [counts (set (vals (frequencies token)))]
    [(if (contains? counts 2)
       1
       0)
     (if (contains? counts 3)
       1
       0)]

    )


 )

; day 2 part 1 sol 1
(apply * (reduce  (fn [ [acc-two acc-three] [two three] ] [(+ two acc-two)
                                                           (+ three acc-three)
                                                           ])  [0 0]  (map score-token (parse data))))

; day 2 part 1 sol 2
(apply * (reduce  matrix/add  (map score-token (parse data))))

(defn delete-char-at
  [^String s ^Long i]
  (.toString (.deleteCharAt (StringBuilder. s) i)))



(defn part-2 [in]
  (first (mapcat (fn [i]
                   (common/duplicates (map #(delete-char-at % i)
                                           in)))
                 (range (count (first in))))))

(part-2 parsed)





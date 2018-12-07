(ns advent.day3
  (:require
    [common :as common]
    [clojure.string :as str]
    [clojure.set :as set]))

(def data (slurp "../AOC3.txt"))



(defn parse
  [s]
  (map (fn
         [l]

         (let [[id x y width height]
               (map
                 #(Long/parseLong %)
                 (rest (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" l)))   ]
           {:id id
            :x x
            :y y
            :width width
            :height height}))
       (str/split-lines s)))


(defn rect-points [{:keys [x y width height] }]
  (for [ i (range x (+ x width))
        j (range y (+ y height))]
    [ i j ]))

(def parsed (parse data))


(def toy-data "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2")



(defn duplicate-points [data]
  (common/duplicates (mapcat rect-points data)))

(def zero (vec (repeat 10 (vec (repeat 10 #{})))))

(defn board [ x y data]
  (reduce
    (fn [b {id :id :as claim}]
      (reduce (fn [b pnt] (update-in b pnt conj id))
              b
              (rect-points claim)))
    (vec (repeat y (vec (repeat x #{}))))
    data ))



(defn print-board [board]
    (doseq [l (apply map vector board)]
      (doseq [ids l]
        (cond
          (empty? ids)
          (print ".")
          (= 1 (count ids))
          (print (first ids))
          :else (print "X")))

      (print "\n")))

(print-board (board 10 10 (parse toy-data)))

;(set/difference (set (map :id data))
;                (set (mapcat #(get-in b %)
;                             (duplicate-points data)))


(defn part-2 [data]
  (let [b (board 1000 1000 data)]

    (set/difference         (set (map :id data))
                            (set (mapcat #(get-in b %)
                                         (duplicate-points data)))           )




    ) )
(part-2 parsed)


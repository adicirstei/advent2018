(ns advent.day4
  (:require

    [clojure.string :as str]))

(def data (slurp "../AOC4.txt"))

(def toy-data "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up")

(let [s toy-data]
  (map (fn [l]
         {:guard-id ...}
         {:wake min}
         {:sleep min})
       (sort (str/split-lines s))
       ))

(defn parse-line [l]
  (let [[_ min op ?gid] (re-find #"\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\] (Guard #(\d+)|falls|wakes)" l)]

    { :min (Long/parseLong min)
     :op (if (str/starts-with? op "Guard") :shift (if (= op "falls") :falls :wakes) )
     :gid (if ?gid  (Long/parseLong ?gid) )
     }))

(defn fill-gid
  [[ph & pr] c]

  (cons (if (and ph (not (:gid c))) (assoc c :gid (:gid ph)) c) (if ph (cons ph pr) nil))

  )

(defn parse [s]
  (reverse (reduce fill-gid [] (map parse-line (sort (str/split-lines s))))))

(def log  (parse data))

(defn aggreg-evts [[prev-sleep res] {op :op min :min}]
  (case op
    :wakes   [nil (cons (range (:min prev-sleep) min) res)]
    :falls   [{:min min} res]
    :shift   [nil res]
    ))





(let [gs (group-by :gid log)]
  (map (fn [[gid evts]]
         (let [ [x sleep-mins] (reduce aggreg-evts [nil nil] evts)
               all-mins (apply concat sleep-mins)]

              [gid  (frequencies all-mins)   ] )
            ) gs)
  )




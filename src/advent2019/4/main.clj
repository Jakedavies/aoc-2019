(ns advent2019.4.main
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

(defn parse-int [s]
  (try
    (Integer. (re-find  #"\d+" s))
    (catch Exception e nil)))

(def ranges [246540 787419])

(parse-int (subs "123" 0 1))

; lol
(def default-counts {1 0
                     2 0
                     3 0
                     4 0
                     5 0
                     6 0
                     7 0
                     8 0
                     9 0})

(defn apply-count [current increment-key]
  "Takes the current count maps and increments the given key"
  (assoc current 
         increment-key
         (+ (get current increment-key) 1)))

(defn is-valid [input]
  "Checks the given input number satisfies the elfs conditions"
  (loop [previous 0
         counts default-counts
         remains (str input)]
    (if (= (count remains) 0)
      (some #(= %1 2) (vals counts))
      (let [current (parse-int (subs remains 0 1))]
        (if (> previous current)
          false
          (recur current
                 (apply-count counts current)
                 (subs remains 1)))))))

; some tests
(is-valid "122233")
(is-valid "1222333")
(is-valid "122330")

(count (filter is-valid (apply range ranges)))

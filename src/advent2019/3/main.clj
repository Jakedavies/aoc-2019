(ns advent2019.2.main
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

(def url  (io/resource "3/input"))
(defn parse-int [s]
  (try
    (Integer. (re-find  #"\d+" s))
    (catch Exception e nil)))

(def initial-input (mapv (fn [input] (str/split input #",")) (str/split-lines (slurp url))))

initial-input

(def wire1 (get initial-input 0))
(def wire2 (get initial-input 1))

(def w1current (atom [0 0]))
(def w1positions (atom {}))
(def stepcount (atom 0))

wire1
(doseq [instruction wire1]
  (println "instruction" instruction)
  (let [direction (subs instruction 0 1)
        total-movement (parse-int (subs instruction 1))]
    (dotimes [i total-movement]
      (swap! stepcount + 1)
      (let [newcurrent (case direction
                         "D" [(get @w1current 0) (- (get @w1current 1) 1)]
                         "U" [(get @w1current 0) (+ 1 (get @w1current 1))]
                         "R" [(+ (get @w1current 0) 1) (get @w1current 1)]
                         "L" [(- (get @w1current 0) 1) (get @w1current 1)])]
        (reset! w1current newcurrent)
        (let [[x y] newcurrent]
          (swap! w1positions assoc [x y] @stepcount))))))

@w1positions

(def w2current (atom [0 0]))
(def w2min (atom 1000000))

(defn abs [n] (max n (- n)))
(defn manhatten [[x y]]
  (+ (abs x) (abs y)))

(reset! stepcount 0)

(doseq [instruction wire2]
  (let [direction (subs instruction 0 1)
        total-movement (parse-int (subs instruction 1))]
    (dotimes [i total-movement]
      (let [newcurrent (case direction
                         "D" [(get @w2current 0) (- (get @w2current 1) 1)]
                         "U" [(get @w2current 0) (+ 1 (get @w2current 1))]
                         "R" [(+ (get @w2current 0) 1) (get @w2current 1)]
                         "L" [(- (get @w2current 0) 1) (get @w2current 1)])]
        (swap! stepcount + 1)
        (let [[x y] newcurrent]
          (if (contains? @w1positions [x y])
            (if (< (+ @stepcount (get @w1positions [x y])) @w2min)
              (reset! w2min (+ @stepcount (get @w1positions [x y]))))))
        (reset! w2current newcurrent)))))

(println @w2min)
@w2min
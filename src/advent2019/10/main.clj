(ns advent2019.10.main
  (:require  [clojure.java.io :as io]
             [clojure.edn :as edn]
             [clojure.string :as str]))

(def url  (io/resource "10/input"))

(defn parse-int [s]
  (try
    (edn/read-string s)
    (catch Exception e nil)))

(def initial-input (map (fn [input] (str/split input #"")) (str/split-lines (slurp url))))

(defn abs [n] (max n (- n)))

(defn angle [[o1 o2] [d1 d2]]
  (Math/atan2 (- o1  d1) (- o2 d2)))

; for every asteroid, we check every other asteroid and then check asteroids where the x and y are between the two current asteroids 

(defn inclusive-range
  ([] (range))
  ([end] (range (inc end)))
  ([start end] (range start (inc end)))
  ([start end step] (range start (+ end step) step)))

(def asteroid-map (into {}  (flatten (for [y (range (count initial-input))]
                                       (for [x (range (count (nth initial-input 0)))]
                                         {[x y] (= "#" (nth (nth initial-input y) x))})))))

(def asteroids  (into {} (filter #(boolean (second %))) asteroid-map))

(defn los [[x1 y1] [x2 y2]]
  (= 1 (count  (->> (let [t (angle [x1 y1] [x2 y2])]
                      (for [x (inclusive-range x1 x2 (if (> x1 x2) -1 1))]
                        (for [y (inclusive-range y1 y2 (if (> y1 y2) -1 1))]
                          (if (and (asteroids [x y]) (= t (angle [x1 y1] [x y])))
                            [x y]
                            false))))

                    (mapcat identity)
                    (filter #(boolean %))))))

(defn count-los [asteroid]
  (reduce
   #(if (los (key asteroid) (key %2)) (+ %1 1) %1)
   0
   asteroids))

(println (apply max (map count-los asteroids)))

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

(defn los [a1 a2]
  ())
; for every asteroid, we check every other asteroid and then check asteroids where the x and y are between the two current asteroids 

(def asteroid-map (into {}  (flatten (for [y (range (count initial-input))]
                                       (for [x (range (count (nth initial-input 0)))]
                                         {[x y] (= "#" (nth (nth initial-input y) x))})))))

(asteroid-map [1 0])
(def asteroids  (into {} (filter #(boolean (second %))) asteroid-map))
asteroids

(asteroids [1 0])

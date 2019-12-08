(ns advent2019.8.main
  (:require  [clojure.java.io :as io]
             [clojure.edn :as edn]
             [clojure.string :as str]))

(def url  (io/resource "8/input"))

(defn parse-int [s]
  (try
    (edn/read-string s)
    (catch Exception e nil)))

(def initial-input (map parse-int (str/split (slurp url) #"")))

(def layers (partition (* 25 6) initial-input))

(get (frequencies  (first layers)) 0)

(def layer-frequencies (map frequencies layers))

(reduce
 #(if (< (get %1 0) (get %2 0)) %1 %2)
 layer-frequencies)

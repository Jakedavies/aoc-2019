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

(def layers   (partition (* 25 6) initial-input))

(def layer-frequencies (map frequencies layers))

(defn write-file [to-write]
  (with-open [w (clojure.java.io/writer  "temp.ppm")]
    (.write w to-write)))

(reduce
 #(if (< (get %1 0) (get %2 0)) %1 %2)
 layer-frequencies)

(defn is-solid [n] (not (= n 2)))

(def flattened-image (for [index (range (count (first layers)))]
                       (loop [layer-index 0]
                         (let [color (nth (nth layers layer-index) index)]
                           (if (is-solid color)
                             color
                             (recur (+ layer-index 1)))))))

(defn to-color [input]
  (case input
    1 "255 255 255 \n"
    0 "0 0 0 \n"))

(write-file (str  "P3\n25 6\n 255\n"  (apply str (map to-color flattened-image))))

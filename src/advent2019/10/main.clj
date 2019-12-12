(ns advent2019.10.main
  (:require  [clojure.java.io :as io]
             [clojure.edn :as edn]
             [clojure.string :as str]))

(defn parse [s]
  (into (sorted-map)
        (for [[y line] (map list (range) (str/split-lines s))
              [x c] (map list (range) line)]
          [[x y] (= c \#)])))

(def input (parse (slurp (io/resource "10/input"))))

(defn dot [a b]
  (apply + (map * a b)))

(defn to-angle [[x y]]
  (Math/atan2 (- x) y))

(defn distance-between [x y]
  (let [y-to-x (mapv - x y)]
    (Math/sqrt (dot y-to-x y-to-x))))

(defn v- [a b]
  (mapv - a b))

(defn angle->viewable [input]
  (let [asteroid-positions (map key (filter val input))]
    (for [current asteroid-positions]
      [current (->> asteroid-positions
                    (remove #{current})
                    (group-by (comp to-angle #(v- current %)))
                    vals
                    count)])))

(defn best-pos [input]
  (apply max-key second (angle->viewable input)))

;; part 1
(best-pos input)

(defn update-vals [f m]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn angle->viewable-by-distance [input current]
  (let [asteroid-positions (map key (filter val input))]
    (->> asteroid-positions
         (remove #{current})
         (group-by (comp to-angle #(v- current %)))
         (update-vals (fn [vs] (sort-by (partial distance-between current) vs)))
         (sort-by key) ; sort by angle
         vec)))

;; part 2
(defn destroyed [input num]
  (let [[best _] (best-pos input)
        angle->meteorites-by-distance (angle->viewable-by-distance input best)
        to-destroy-count (dec (count (filter val input)))
        index-of-up (.indexOf (mapv first angle->meteorites-by-distance) 0.0)
        meteorites (mapv second angle->meteorites-by-distance)
        n (count (mapcat identity meteorites))
        destroy-order
        (loop [meteorites meteorites
               i index-of-up
               destroyed []]
          (let [index (mod i n)
                meteorites-at-angle (get meteorites index)]
            (cond
              (seq meteorites-at-angle)
              (recur (assoc meteorites index (rest meteorites-at-angle))
                     (inc i)
                     (conj destroyed (first meteorites-at-angle)))
              (= (count destroyed) to-destroy-count)
              destroyed
              :else
              (recur meteorites (inc i) destroyed))))
        [x y] (get destroy-order (dec num))]
    (+ (* x 100) y)))

(println  {:part-1 (best-pos input)
           :part-2 (destroyed input 200)})

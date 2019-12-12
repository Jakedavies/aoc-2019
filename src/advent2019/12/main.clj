(ns advent2019.12.main
  (:require  [clojure.java.io :as io]
             [clojure.edn :as edn]
             [clojure.string :as str]))

(def url  (io/resource "12/input"))

; <x=4, y=1, z=1>
; <x=11, y=-18, z=-1>
; <x=-2, y=-10, z=-4>
; <x=-7, y=-2, z=14>


(def init (map
           #(merge %1 {:xv 0 :yv 0 :zv 0})
           (list
            {:x 4 :y 1 :z 1}
            {:x 11 :y -18 :z -1}
            {:x -2 :y -10 :z -4}
            {:x -7 :y -2 :z 14})))

(defn calc [cur p1 p2]
  (+ cur (if (= p1 p2)
           0
           (if (> p1 p2) -1 1))))

(defn apply-gravity [moon1 moon2]
  (-> moon1
      (assoc :xv (calc (:xv moon1) (:x moon1) (:x moon2)))
      (assoc :yv (calc (:yv moon1) (:y moon1) (:y moon2)))
      (assoc :zv (calc (:zv moon1) (:z moon1) (:z moon2)))))

(apply-gravity (nth init 0) (nth init 1))

(defn update-velocity [moon other-moons]
  (reduce
   #(apply-gravity %1 %2)
   moon
   other-moons))

(update-velocity (first init) (rest init))

(defn position [moon other-moons])

(defn splice [l index] (concat
                        (take index l)
                        (take-last (- (count l) (+ index 1)) l)))

(defn update-position [moon]
  (-> moon
      (assoc :x (+ (:x moon) (:xv moon)))
      (assoc :y (+ (:y moon) (:yv moon)))
      (assoc :z (+ (:z moon) (:zv moon)))))

(splice init 3)

(defn simulate [moons]
  (->> (range (count moons))
       (map #(update-velocity (nth moons %1) (splice moons %1)))
       (map #(update-position %1))))

(simulate init)

(defn simulate-n [max-iterations initial-state]
  (loop [i 0 s initial-state]
    (if (< i max-iterations)
      (recur (+ i 1) (simulate s))
      s)))

(defn abs [n] (max n (- n)))

(defn energy [moon]
  (* (apply + (map abs (take 3 (vals moon))))
     (apply + (map abs (take-last  3 (vals moon))))))

(simulate-n 1000 init)

;<x=-8, y=-10, z=0>
;<x=5, y=5, z=10>
;<x=2, y=-7, z=3>
;<x=9, y=-8, z=-3>

(def sample1 (map
              #(merge %1 {:xv 0 :yv 0 :zv 0})
              (list
               {:x -8 :y -10 :z 0}
               {:x 5 :y 5 :z 10}
               {:x 2 :y -7 :z 3}
               {:x 9 :y -8 :z -3})))

;; produce energy value?
(println  (time (reduce
                 #(+ %1 (energy %2))
                 0
                 (simulate-n 100000 init))))

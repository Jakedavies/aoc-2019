(ns advent2019.3.main
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

(def url  (io/resource "3/input"))
(defn parse-int [s]
  (try
    (Integer. (re-find  #"\d+" s))
    (catch Exception e nil)))

(def initial-input (mapv (fn [input] (str/split input #",")) (str/split-lines (slurp url))))

(def wire1 (get initial-input 0))
(def wire2 (get initial-input 1))

(def w1current (atom [0 0]))
(def w1positions (atom {}))
(def stepcount (atom 0))


(def move 
  {\D [0 -1]
   \U [0 1]
   \L [-1 0]
   \R [1 0]
   })

(def parse
  (juxt first #(parse-int (subs % 1))))

(parse "D333")
(mapv + [1 1] [1 2])

(defn apply-move [current transform]
  (mapv + current transform))

(#(apply-move %1 %2) [1 2] [-1 0])

(defn trace
  "advances the trace from the current trace list by the given [direction distance]"
  {:test #(do
            (assert (= (trace [[1 10]] [\D 3]) [[1 10] [1 9] [1 8] [1 7]])))}

  ([current-path [direction distance]]
   (reductions
    #(apply-move %1 %2)
    (last current-path)
    (repeat distance (move direction)))))

(doseq [instruction wire1]
  (let [[direction movement] (parse instruction)]
    (dotimes [i movement]
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

(defn manhatten [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(manhatten [-10 10])

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

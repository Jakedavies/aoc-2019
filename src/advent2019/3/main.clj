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
   \R [1 0]})

(def parse
  (juxt first #(parse-int (subs % 1))))

(parse "D333")
(mapv + [1 1] [1 2])

(defn apply-move
  "Takes a current position and a transformation vector to produce the new position"
  {:test #(do
            (assert (= (apply-move [1 2] [-1 0]) [0 2])))}
  ([current transform] (mapv + current transform)))

(test #'apply-move)

(defn better-concat [list1 list2]
  (reduce
   #(conj %1 %2)
   list1
   list2))

(better-concat [1] [1 2])

(defn trace
  "advances the trace from the current trace list by the given [direction distance]"
  {:test #(do
            (assert (=
                     (trace [[1 2] [1 10]] [\D 3])
                     [[1 2] [1 10] [1 9] [1 8] [1 7]])))}

  ([current-path [direction distance]]
   (println "running " direction distance)
   (into current-path  (rest  (reductions
                                 #(apply-move %1 %2)
                                 (peek current-path)
                                 (repeat distance (move direction)))))))

(test #'trace)
(trace  '((0 103)) [\D 3])

;; wont run slows exponentially until stack overflow
(time (count  (reduce
               #(into %1 (trace %1 (parse %2)))
               '((0 0))
               (subvec  wire1 0 13))))

;; completes in a reasonable amount of time
(time  (count (transduce (map parse) (completing trace) '((0 0)) wire1)))


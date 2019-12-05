(ns advent2019.5.main
  (:require  [clojure.java.io :as io]
             [clojure.edn :as edn]
             [clojure.string :as str]))

(def url  (io/resource "5/input"))

(defn parse-int [s]
  (try
    (edn/read-string s)
    (catch Exception e nil)))

(parse-int "-1")

(def initial-input (mapv parse-int (str/split (slurp url) #",")))

(defn instruction [input]
  (reverse input))

(defn instruction-components [input]
  [(rem input 100)
   (rem (quot input 100) 10)
   (rem (quot input 1000) 10)
   (rem (quot input 10000) 10)])

(assert (= (instruction-components 3) [3 0 0 0]))
(assert (= (instruction-components 1002) [2 0 1 0]))

(assoc [1 2 3 4] 2 69)
(get [1 2 3 4 5] 10)

(if 0
  true
  false)

(defn eval-with-mode [state mode r]
  (if (= mode 1) r (get state r)))

(defn add [state [r1 mode1] [r2 mode2]]
  (+ (eval-with-mode state mode1 r1)
     (eval-with-mode state mode2 r2)))

(defn mult [state [r1 mode1] [r2 mode2]]
  (* (if (= mode1 1) r1 (get state r1))
     (if (= mode2 1) r2 (get state r2))))

(def constant-input 5)

(defn apply-instruction [counter state]
  (let [[opcode mode1 mode2 mode3] (instruction-components (get state counter))]
    (let [v1 (get state (+ counter 1))
          v2 (get state (+ counter 2))
          v3 (get state (+ counter 3))]
      (case opcode
        1 [(+ counter 4) (assoc state v3 (add state [v1 mode1] [v2 mode2]))]
        2 [(+ counter 4) (assoc state v3 (mult state [v1 mode1] [v2 mode2]))]
        3 [(+ counter 2) (assoc state v1 constant-input)]
        4 [(+ counter 2) (do (println "output: " (if (= mode1 1) v1 (get state v1)))
                             state)]
        5 (if (not= (eval-with-mode state mode1 v1) 0)
            [(eval-with-mode state mode2 v2) state]
            [(+ counter 3) state])
        6 (if (= (eval-with-mode state mode1 v1) 0)
            [(eval-with-mode state mode2 v2) state]
            [(+ counter 3) state])
        7 [(+ counter 4)
           (assoc state v3
                  (if (< (eval-with-mode state mode1 v1) (eval-with-mode state mode2 v2)) 1 0))]

        8 [(+ counter 4)
           (assoc state v3
                  (if (= (eval-with-mode state mode1 v1) (eval-with-mode state mode2 v2)) 1 0))]
        99 [(+ counter 100000000000) state]))))

(assert (apply-instruction 0 [1002 4 3 4 33])
        [4 [1002 4 3 4 99]])

(assert (apply-instruction 0 [0101 4 3 4 33])
        [4 [1002 4 3 4 99]])

(loop [program-counter 0
       state initial-input]
  (if  (> program-counter (count state))
    state
    (let [[new-counter new-state] (apply-instruction program-counter state)]
      (recur new-counter new-state))))

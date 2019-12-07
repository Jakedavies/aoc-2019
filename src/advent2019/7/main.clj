(ns advent2019.7.main
  (:require  [clojure.java.io :as io]
             [clojure.edn :as edn]
             [clojure.string :as str]))

(def url  (io/resource "7/input"))

(defn parse-int [s]
  (try
    (edn/read-string s)
    (catch Exception e nil)))

(parse-int "-1")

(def initial-input (mapv parse-int (str/split (slurp url) #",")))

(defn instruction-components [input]
  [(rem input 100)
   (rem (quot input 100) 10)
   (rem (quot input 1000) 10)
   (rem (quot input 10000) 10)])

(assert (= (instruction-components 3) [3 0 0 0]))
(assert (= (instruction-components 1002) [2 0 1 0]))

(defn eval-with-mode [state mode r]
  (if (= mode 1) r (get state r)))

(defn apply-instruction [counter state get-input output-fn]
  (let [[opcode mode1 mode2 mode3] (instruction-components (get state counter))]
    (let [v1 (get state (+ counter 1))
          v2 (get state (+ counter 2))
          v3 (get state (+ counter 3))
          f1 (eval-with-mode state mode1 v1)
          f2 (eval-with-mode state mode2 v2)]
      (case opcode
        1 [(+ counter 4) (assoc state v3 (+ f1 f2))]
        2 [(+ counter 4) (assoc state v3 (* f1 f2))]
        3 [(+ counter 2) (assoc state v1 (get-input))]
        4 [(+ counter 2) (do (output-fn f1)
                             state)]
        5 (if (not= f1 0)
            [f2 state]
            [(+ counter 3) state])
        6 (if (zero? f1)
            [f2 state]
            [(+ counter 3) state])
        7 [(+ counter 4)
           (assoc state v3 (if (< f1 f2) 1 0))]
        8 [(+ counter 4)
           (assoc state v3 (if (= f1 f2) 1 0))]
        99 [(+ counter 100000000000) state]))))

(def itest (atom  [1 2]))
(pop @itest)

(defn run [i]
  (let [inputs (atom i)
        outputs (atom [])
        get-input #(let [n (first @inputs)]
                     (reset! inputs (rest @inputs))
                     n)
        output #(swap! outputs conj %1)]
    (loop [program-counter 0
           state initial-input]
      (if  (> program-counter (count state))
        state
        (let [[new-counter new-state] (apply-instruction program-counter state get-input output)]
          (recur new-counter new-state))))
    (first @outputs)))

(defn run-phases [i1 i2 i3 i4 i5]
  (-> 0
      (#(run [i1 %1]))
      (#(run [i2 %1]))
      (#(run [i3 %1]))
      (#(run [i4 %1]))
      (#(run [i5 %1]))))

(run [4 0])
(run [4 0])

(run-phases 5 5 5 5 4)

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(apply max (mapv #(apply run-phases %1) (permutations (range 0 5))))

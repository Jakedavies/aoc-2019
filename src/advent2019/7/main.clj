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

(defn apply-instruction []
  (merge i
         (let [{:keys [input state counter output]} i]
           (let [[opcode mode1 mode2 mode3] (instruction-components (get state counter))]
             (let [v1 (get state (+ counter 1))
                   v2 (get state (+ counter 2))
                   v3 (get state (+ counter 3))
                   f1 (eval-with-mode state mode1 v1)
                   f2 (eval-with-mode state mode2 v2)]
               (case opcode
                 1 {:counter (+ counter 4) :state (assoc state v3 (+ f1 f2))}
                 2 {:counter (+ counter 4) :state (assoc state v3 (* f1 f2))}
                 3 (if (empty? input)
                     {:counter counter :state state :awaiting-input true}
                     {:input (rest input) :counter (+ counter 2) :state (assoc state v1 (first input))})
                 4 {:counter (+ counter 2) :state state :output f1}
                 5 (if (not= f1 0)
                     {:counter  f2 :state state}
                     {:counter (+ counter 3) :state state})
                 6 (if (zero? f1)
                     {:counter f2 :state state}
                     {:counter (+ counter 3) :state state})
                 7 {:counter (+ counter 4)
                    :state (assoc state v3 (if (< f1 f2) 1 0))}
                 8 {:counter (+ counter 4)
                    :state (assoc state v3 (if (= f1 f2) 1 0))}
                 99 {:counter (+ counter 2) :state state :halted true}))))))

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

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

; if the computer is awaiting input-> move forward to the next computer
; run until the computer halts or await input
; if halts remove from list
; if awaiting input, advance to next computer

(defn run [[computer & rest-computers :as computers] inputs]
  (if-not computer
    (first inputs)
    (let [computer' (-> computer
                        (dissoc :awaiting-input)
                        (update :input concat inputs)
                        (->> (iterate apply-instruction)
                             (drop-while #(not (or (:halted %) (:awaiting-input %))))
                             first))
          outputs (:output computer')
          computers' (cond
                       (:halted computer')
                       rest-computers
                       (:awaiting-input computer')
                       (conj (vec rest-computers) (assoc computer' :output [])))]
      (recur computers' outputs))))

(defn initial-amplifier
  [p phase-input]
  {:state p :counter 0 :input [phase-input] :output []})

(run [(initial-amplifier initial-input 4)] [0])

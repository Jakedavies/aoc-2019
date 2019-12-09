(ns advent2019.9.main
  (:require  [clojure.java.io :as io]
             [clojure.edn :as edn]
             [clojure.string :as str]))

(def url  (io/resource "9/input"))

(defn parse-int [s]
  (try
    (edn/read-string s)
    (catch Exception e nil)))

(def initial-input (mapv parse-int (str/split (slurp url) #",")))

(defn instruction-components [input]
  [(rem input 100)
   (rem (quot input 100) 10)
   (rem (quot input 1000) 10)
   (rem (quot input 10000) 10)])

(assert (= (instruction-components 3) [3 0 0 0]))
(assert (= (instruction-components 1002) [2 0 1 0]))

(defn eval-with-mode [state mode r relative-base]
  (if (= 2 mode)
    (do (println "relative-base " relative-base r)
        (println (get state (+ relative-base r)))))
  (case mode
    2 (get state (+ relative-base r))
    1 r
    0 (get state r)))

(defn apply-instruction [i]
  (merge i
         (let [{:keys [input state counter output relative-base]} i]
           (let [[opcode mode1 mode2 mode3] (instruction-components (get state counter))]
             (if  (= mode3 2) (println "Non 0 write code for r3" opcode mode3))
             (if  (= mode2 2) (println "Non 0 write code for r2" opcode mode2))
             (if  (= mode1 2) (println "Non 0 write code for r1" opcode mode1))
             (let [v1 (get state (+ counter 1))
                   v2 (get state (+ counter 2))
                   v3 (get state (+ counter 3))
                   f1 (eval-with-mode state mode1 v1 relative-base)
                   f2 (eval-with-mode state mode2 v2 relative-base)]
               (case opcode
                 1 {:counter (+ counter 4) :state (assoc state v3 (+ f1 f2))}
                 2 {:counter (+ counter 4) :state (assoc state v3 (* f1 f2))}
                 3 (if (empty? input)
                     {:counter counter :state state :awaiting-input true}
                     {:input (rest input) :counter (+ counter 2) :state (assoc state v1 (first input))})
                 4 {:counter (+ counter 2) :state state :output (conj output f1)}
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
                 9 {:counter (+ counter 2)
                    :relative-base (+ relative-base f1)}
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

;; phases runner
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

(def p1 [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99])
(def p2 [1102 34915192 34915192 7 4 7 99 0])
(def p3 [104 1125899906842624 99])

(defn pad-memory [cur]
  (into [] (concat cur (map (constantly 0) (range (- 10000 (count cur)))))))

(defn initial-state
  [state input]
  {:state (pad-memory state) :counter 0 :input input  :output [] :relative-base 0})

(:output (-> (initial-state initial-input [1])
             (->> (iterate apply-instruction)
                  (drop-while #(not (or (:halted %) (:awaiting-input %))))
                  first)))

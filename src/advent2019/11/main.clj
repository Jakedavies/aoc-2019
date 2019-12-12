(ns advent2019.11.main
  (:require  [clojure.java.io :as io]
             [clojure.edn :as edn]
             [clojure.string :as str]))

(def url  (io/resource "11/input"))

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
  (case mode
    2 (get state (+ relative-base r))
    1 r
    0 (get state r)))

(defn eval-with-mode-output [state mode r relative-base]
  (case mode
    2 (+ relative-base r)
    1 nil
    0 r))

(defn apply-instruction [i]
  (merge i
         (let [{:keys [input state counter output relative-base]} i]
           (let [[opcode mode1 mode2 mode3] (instruction-components (get state counter))]
             (let [v1 (get state (+ counter 1))
                   v2 (get state (+ counter 2))
                   v3 (get state (+ counter 3))
                   o1 (eval-with-mode-output state mode1 v1 relative-base)
                   o2 (eval-with-mode-output state mode2 v2 relative-base)
                   o3 (eval-with-mode-output state mode3 v3 relative-base)
                   f1 (eval-with-mode state mode1 v1 relative-base)
                   f2 (eval-with-mode state mode2 v2 relative-base)
                   f3 (eval-with-mode state mode3 v3 relative-base)]
               (case opcode
                 1 {:counter (+ counter 4) :state (assoc state o3 (+ f1 f2))}
                 2 {:counter (+ counter 4) :state (assoc state o3 (* f1 f2))}
                 3 (if (empty? input)
                     {:counter counter :state state :awaiting-input true}
                     {:input (rest input) :counter (+ counter 2) :state (assoc state o1 (first input))})
                 4 {:counter (+ counter 2) :state state :output (conj output f1)}
                 5 (if (not= f1 0)
                     {:counter  f2 :state state}
                     {:counter (+ counter 3) :state state})
                 6 (if (zero? f1)
                     {:counter f2 :state state}
                     {:counter (+ counter 3) :state state})
                 7 {:counter (+ counter 4)
                    :state (assoc state o3 (if (< f1 f2) 1 0))}
                 8 {:counter (+ counter 4)
                    :state (assoc state o3 (if (= f1 f2) 1 0))}
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

(defn pad-memory [cur]
  (into [] (concat cur (map (constantly 0) (range (- 10000 (count cur)))))))

(defn initial-state
  [state input]
  {:state (pad-memory state) :counter 0 :input input  :output [] :relative-base 0})

(def move
  {\U [0 -1]
   \L [-1 0]
   \D [0 1]
   \R [1 0]})

(def directions
  {\U {0 \L 1 \R}
   \L {0 \D 1 \U}
   \D {0 \R 1 \L}
   \R {0 \U 1 \D}})

(def black 0)
(def white 1)

(directions \D)

(def dd {[0 0] 0, [2 3] 5})

(assoc dd [0 0] 69)
(assoc dd [0 0] 70)

(dd [2 3])

(if 0 "nil is truth" "nil is falsy")
(def max-iterations 1000000)

(def board-state  (loop [board-state {}
                         current-direction \U
                         current-position [0 0]
                         iterations 0
                         current-color white
                         program-state (initial-state initial-input [current-color])]
                    (let [current-state (-> program-state
                                            (assoc :input [current-color])
                                            (dissoc :awaiting-input)
                                            (assoc :output [])
                                            (->> (iterate apply-instruction)
                                                 (drop-while #(not (or (:halted %) (:awaiting-input %))))
                                                 first))
                          output (:output current-state)
                          halted (:halted current-state)]
                      (if (or halted (>  iterations max-iterations))
                        board-state
                        (do
                          (let [new-direction ((directions current-direction) (second output))
                                new-position (into [] (map + current-position (move new-direction)))
                                color-at-position (board-state new-position)]
                            (recur
                             (assoc board-state current-position (first output))
                             new-direction
                             new-position
                             (+ 1 iterations)
                             (if color-at-position color-at-position black)
                             current-state)))))))

(def scale (let [ks (keys board-state)
                 vs (vals board-state)
                 max-x (apply max (map first ks))
                 min-x (apply min (map first ks))
                 max-y (apply max (map second ks))
                 min-y (apply min (map second ks))]
             {:minx min-x
              :miny min-y
              :scale-x (+ 1 max-x)
              :scale-y (+ 1 max-y)}))
scale
(defn to-color [input]
  (case input
    0 "255 255 255 \n"
    1 "0 0 0 \n"))

(defn add-color [acc coord]
  (let [scaled-x (first coord)
        scaled-y (second coord)
        color (board-state [scaled-x scaled-y])]
    (conj acc (if color color black))))

(defn write-file [to-write]
  (with-open [w (clojure.java.io/writer  "temp.ppm")]
    (.write w to-write)))

(write-file (str  "P3\n43 6\n 255\n"  (apply str (map to-color
                                                      (reduce
                                                       (fn [acc y]
                                                         (reduce #(add-color %1 [%2 y]) acc (range (:scale-x scale))))
                                                       []
                                                       (range (:scale-y scale)))))))

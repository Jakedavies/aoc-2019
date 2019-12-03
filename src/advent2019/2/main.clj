(ns advent2019.2.main
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

(def url  (io/resource "2/input"))

(defn parse-int [s]
  (try
    (Integer. (re-find  #"\d+" s))
    (catch Exception e nil)))

; runs program on input list and returns the modified list
(defn run-program [input]
  (let [program (atom input)]
    (doseq [[opcode r1 r2 outputpos] (partition 4 @program)]
      (let [r1val (get @program r1)
            r2val (get @program r2)]
        (case opcode
          1 (reset! program
                    (assoc @program
                           outputpos
                           (+ r1val r2val)))
          2 (reset! program (assoc @program outputpos (* r1val r2val)))
          99 (println "halt"))
        (println @program)))
    @program))

(def initial-input (mapv parse-int (str/split (slurp url) #",")))

(doseq [noun (range 99)]
  (doseq [verb (range 99)]
    (if (= 19690720
           (-> initial-input
               ; set the verb and the noun, then run
               (assoc 1 noun)
               (assoc 2 verb)
               (run-program)
               (get 0)))
      (println noun verb))))


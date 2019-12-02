(ns advent2019.2.main
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

(def url  (io/resource "2/input"))

(defn process-file [filename process-fn]
  (with-open [reader (io/reader filename)]
    (doseq [line (line-seq reader)]
      (process-fn line))))

(defn parse-int [s]
  (try
    (Integer. (re-find  #"\d+" s))
    (catch Exception e nil)))

(defn process-line [line]
  (let [program (atom (mapv  parse-int (str/split  line #",")))]
 ;   (reset! program (assoc @program 1 12))
 ;   (reset! program (assoc @program 2 2))
    (doseq [[opcode r1 r2 outputpos] (partition 4 @program)]
      (println opcode r1 r2 outputpos)
      (let [r1val (get @program r1)
            r2val (get @program r2)]
        (case opcode
          1 (reset! program
                    (assoc @program
                           outputpos
                           (+ r1 r2)))
          2 (reset! program (assoc @program outputpos (* r1 r2)))
          99 (println "halt"))
        (println @program)))))

(process-line "1,9,10,3,2,3,11,0,99,30,40,50")
(process-file url process-line)

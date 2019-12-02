(ns advent2019.1.main
  ( :require  [clojure.java.io :as io]))

(defn required [mass]
  (if (< mass 0)
      0
      (+ mass  (required  (- (int (Math/floor (/ mass 3))) 2)))))


(def url  (io/resource "1/input"))

(defn process-file [filename process-fn]
  (with-open [reader (io/reader filename)]
    (doseq [line (line-seq reader)]
        (process-fn line))))

(def module-mass (atom 0))

(- (required 1969) 1969)

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(process-file url
              (fn [mass]
                (let [mass-int (parse-int mass)]
                (reset! module-mass
                       (+ @module-mass (- (required mass-int) mass-int))))))

@module-mass

(println @module-mass)

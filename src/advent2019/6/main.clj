(ns advent2019.6.main
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

; 4k8 

(defn add-edge [graph-state [v1 v2]]
  (-> graph-state
      (assoc-in [v1 :children] (conj (:children (get graph-state v1)) v2))
      (assoc-in [v2 :parent] v1)))

(def url  (io/resource "6/input"))
(def initial-input (mapv  #(str/split %1 #"\)") (str/split-lines (slurp url))))
(def initial-node-state {:children []})

; build a
(defn build-graph [edges]
  (reduce
   (fn [g [v1 v2]] (add-edge g [(keyword v1) (keyword v2)]))
   {}
   edges))

(def graph (build-graph initial-input))

(defn count-orbits [node]
  (loop [orbit-count 0
         current-node node]
    (if (boolean (:parent current-node))
      (recur (+ 1 orbit-count) ((:parent current-node) graph))
      orbit-count)))

; part one
(transduce
 (map count-orbits)
 +
 0
 (vals graph))

(defn list-contains [search-key a-list]
  (boolean (some #{search-key} a-list)))

(defn search-next-nodes [to-explore-keys]
  (reduce
   #(concat %1 [(:parent %2)] (:children %2))
   []
   (map #(%1 graph) to-explore-keys)))

(def start (:parent (:YOU graph)))
(def destination (:parent (:SAN graph)))

; part 2
(loop [jumps 0
       already-explored [:YOU]
       explore-list [start]]
  (if (list-contains destination explore-list)
    jumps
    (if (zero? (count explore-list))
      nil
      (recur (+ jumps 1)
             (concat already-explored explore-list)
             (filter #(not (list-contains %1 already-explored)) (search-next-nodes (filter boolean explore-list)))))))

(ns lseq-tree.identifier
  (:refer-clojure :exclude  [compare])
  (:require [lseq-tree.base :refer [base bit sum]]
            [lseq-tree.lseq-node :refer [node]]
            [lseq-tree.triple :refer [triple]]))

;; TODO abstract the math away so it is js and java compliant

(defn pow [n x]
  (Math/pow n x))

(defrecord ID [digit site counter])

(defn id [digit site counter]
  (->ID digit site counter))

(defn depth
  ([node] (depth node 1))
  ([node acc]
   (if (= (count (:children node)) 0)
     acc
     (recur (first (:children node)) (inc acc)))))

(defn node->id
  ([node] (node->id node (base)))
  ([node base]
   (let [length (depth node)]
     (loop [{:keys [children]
             {:keys [path site counter]} :triple} node
            id-digit 0
            id-site []
            id-counter []
            level 0]
       (if (< level length)
         (let [next-digit (+ id-digit path)]
           (recur (first children)
                  (if-not (= level (dec length))
                    (bit-shift-left next-digit
                                    (bit base (inc level)))
                    next-digit)
                  (conj id-site site)
                  (conj id-counter counter)
                  (inc level)))
         (id id-digit id-site id-counter))))))

(defn digit->path
  [digit base level bit-length]
  (int (mod (bit-shift-right digit
                             (- bit-length (sum base level)))
            (pow 2 (bit base level)))))

(defn id->node
  ([id] (id->node id (base)))
  ([{:keys [digit site counter] :as id} base]
   (let [length (count counter)
         bit-length (sum base (- length 1))]
     (loop [triples []
            level 0]
       (if (< level length)
         (recur (conj triples
                      (triple (digit->path
                                digit base level bit-length)
                              (nth site level)
                              (nth counter level)))
                (inc level))
         (node triples nil))))))

(ns lseq-tree.strategy
  (:require [lseq-tree.base :refer [sum bit base]]
            [lseq-tree.identifier :refer [id]]))

(defn pow
  [n x]
  (Math/pow n x))

(defn next-id
  ([digit prev-node next-node level site counter]
   (next-id digit prev-node next-node level site counter (base)))
  ([digit
    prev-node next-node
    level
    site counter base]
   (let [sum-bit (sum base level)]
     (loop [sites []
            counters []
            {{prev-path :path
              prev-site :site
              prev-counter :counter} :triple
             prev-children :children} prev-node
            {{next-path :path
              next-site :site
              next-counter :counter} :triple
             next-children :children} next-node
            depth 0]
       (if (<= depth level)
         (let [path
               (int (mod (bit-shift-right
                           digit (- sum-bit (sum base depth)))
                         (pow 2 (bit base depth))))
               [next-sites next-counters]
               (cond
                 (= path next-path) [(conj sites next-site)
                                     (conj counters next-counter)]
                 (= path prev-path) [(conj sites prev-site)
                                     (conj counters prev-counter)]
                 :default [sites counters])]
           (recur next-sites
                  next-counters
                  (first prev-children)
                  (first next-children)
                  (inc depth)))
         (id digit
             (conj sites site)
             (conj counters counter)))))))

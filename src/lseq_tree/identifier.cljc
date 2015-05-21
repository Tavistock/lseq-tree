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
            id-digit 0, id-site [], id-counter []
            level 0]
       (if (< level length)
         (let [next-digit (+ id-digit path)
               digit (if-not (= level (dec length))
                       (bit-shift-left
                         next-digit
                         (bit base (inc level)))
                       next-digit)]
           (recur (first children)
                  digit (conj id-site site) (conj id-counter counter)
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

(def secondary-values
  (juxt :site :counter))

(defn compare
  ([id other] (compare id other (base)))
  ([id other base]
  (let [id-length        (count (:counter id))
        other-length     (count (:counter other))
        min-length       (min id-length other-length)
        id-bit-length    (sum base (dec id-length))
        other-bit-length (sum base (dec other-length))]
  (loop [level 0]
    (if (>= level min-length)
      (- id-length other-length)
      (let [sum-bit (sum base level)
          id-primary (bit-shift-right
                       (:digit id)
                       (- id-bit-length sum-bit))
          other-primary (bit-shift-right
                          (:digit other)
                          (- other-bit-length sum-bit))
          id-2nd    (nth (:site id) level)
          other-2nd (nth (:site other) level)
          id-3rd    (nth (:counter id) level)
          other-3rd (nth (:counter other) level)]
        (if-not (= id-primary other-primary)
          (- id-primary other-primary)
          (if-not (= id-2nd other-2nd)
            (- id-2nd other-2nd)
            (if-not (= id-3rd other-3rd)
              (- id-3rd other-3rd)
              (recur (inc level)))))))))))

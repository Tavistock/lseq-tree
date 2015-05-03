(ns lseq-tree.base)

(defprotocol IBitsum
  (bit [this n])
  (sum [this level]))

(defrecord Base [base]
  IBitsum
  (bit [this n]
    (+ (:base this) n))
  (sum [this level]
    (let [x (bit this level)
          y (- (:base this) 1)]
      (- (/ (* x (+ x 1)) 2)
         (/ (* y (+ y 1)) 2)))))

(defn base
  ([] (base 3))
  ([n] (Base. n)))

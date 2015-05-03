(ns lseq-tree.base)

(defrecord Base [base])

(defn new-base
  ([] (new-base 3))
  ([n] (Base. n)))

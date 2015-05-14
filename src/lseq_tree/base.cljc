(ns lseq-tree.base)

(defrecord Base [size])

(defn bit
  [{:keys [size]} n]
  (+ size n))

(defn sum
  [{:keys [size] :as base} level]
  (let [x (bit base level)
        y (- size 1)]
    (- (/ (* x (+ x 1)) 2)
       (/ (* y (+ y 1)) 2))))

(defn base
  ([] (base 3))
  ([n] (Base. n)))

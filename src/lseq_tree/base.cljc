(ns lseq-tree.base)

(defn pow
  [n x]
  (Math/pow n x))

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

(def node-val
  (comp :path :triple))

(def fchild
  (comp first :children))

(defn interval
  [base prev-node next-node level]
  (loop [prev prev-node, post next-node
         i 0, acc 0, common-root true, prev-greater false]
    (if (> i level) acc
      (let [prev-val (or (node-val prev) 0)
            proposed-val (or (node-val post) 0)
            diverge? (and common-root
                          (not (== prev-val proposed-val)))
            common-root (if diverge? false common-root)
            prev-greater (if diverge?
                           (> prev-val proposed-val)
                           prev-greater)
            post-val (if prev-greater
                       (- (pow 2 (bit base i)) 1)
                       proposed-val)
            acc (if (or common-root
                        prev-greater
                        (not= i level))
                  (+ acc (- post-val prev-val))
                  (+ acc (- post-val prev-val 1)))
            acc (if (not= i level)
                  (* acc (pow 2 (bit base (+ i 1))))
                  acc)]
        (recur (fchild prev), (fchild post)
               (inc i) acc, common-root, prev-greater)))))

(defn base
  ([] (base 3))
  ([n] (Base. n)))

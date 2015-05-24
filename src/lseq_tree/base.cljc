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

(defn interval
  [base lower upper level]
  (loop [lower lower, upper upper
         depth 0, acc 0, common-root true, low-greater false]
    (if (> depth level)
      acc
      (let [low-val (or (-> lower :triple :path) 0)
            up-val (or (-> upper :triple :path) 0)
            diverge? (and common-root (not (== low-val up-val)))
            common-root (if diverge? false common-root)
            low-greater (if diverge? (> low-val up-val) low-greater)
            up-val1 (if low-greater (dec (pow 2 (bit base depth))) up-val)
            acc1  (+ acc (- up-val1 low-val (if (or common-root
                                                    low-greater
                                                    (not= depth level)) 0 1)))
            acc2 (if (not= depth level)
                   (* acc1 (pow 2 (bit base (+ depth 1))))
                   acc1)]
        (recur (-> lower :children first), (-> upper :children first)
               (inc depth) acc2, common-root, low-greater)))))

(defn base
  ([] (base 3))
  ([n] (Base. n)))

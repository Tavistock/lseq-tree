(ns lseq-tree.util)

(defmulti direction (fn [x _] (class x)))

(defn get-middle
  [collection]
  (Math/round (Math/floor (/ (count collection) 2))))

(defn binary-search
  "Searches in the collection(xs) for the element(e)
  the collection and element must implement the direction protocol"
  [xs e]
  (loop [e e, xs xs, idx-mod 0]
    (let [mid-idx (get-middle xs)
          mid-val (nth xs mid-idx)]
      (if (= e mid-val)
        (+ mid-idx idx-mod)
        (if (= mid-idx 0)
          nil
          (condp = (direction mid-val e)
            1 (recur e
                      (subvec xs 0 mid-idx)
                      idx-mod)
            -1 (recur e
                     (subvec xs (inc mid-idx) (count xs))
                     (+ mid-idx idx-mod 1))
            0 nil))))))


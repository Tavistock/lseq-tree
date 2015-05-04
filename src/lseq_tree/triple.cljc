(ns lseq-tree.triple
  (:require [lseq-tree.util :refer [direction]]))

(defrecord Triple [path site counter])

(defmethod direction Triple
  [{p1 :path s1 :site c1 :counter}
   {p2 :path s2 :site c2 :counter}]
  "compare two triples prioritizing the path, then site, then counter
  it either returns -1, 0, or 1"
  (cond
    (< p1 p2) -1
    (> p1 p2) 1
    :else (cond
            (< s1 s2) -1
            (> s1 s2) 1
            :else (cond
                    (< c1 c2) -1
                    (> c1 c2) 1
                    :else 0))))

(defn triple
  ([] (triple 0 0 0))
  ([path site counter] (Triple. path site counter)))

(ns lseq-tree.triple)

(defrecord Triple [path site counter])

(defn sorted
  "compare two triples prioritizing the path, then site, then counter
  it either returns -1, 0, or 1"
  ([xs] (sort-by (juxt :path :site :counter) xs))
  ([k xs] (sort-by #((juxt :path :site :counter) (k %)) xs )))

(defn triple
  ([] (triple 0 0 0))
  ([path site counter] (Triple. path site counter)))

(ns lseq-tree.lseq-node
  (:require [lseq-tree.triple :as t :refer [sorted]]
            [clojure.zip :as z]))

(defn find-in [xs n]
  (.indexOf xs n))

(defn node-zip
  [root]
  (z/zipper
    (fn [_] true)
    (fn [n] (if (vector? n)
              (seq n)
              (:children n)))
    (fn [n c] nil) ;do not use any editing on the tree
    root))

(defn crawl-to
  "returns a zipper at the nth element"
  [node index]
  (loop [zip (node-zip node)
         to-go index]
    (let [elem (:element (z/node zip))]
      (if (and (= to-go 0) elem)
        zip
        (if (and (z/end? zip) (>= to-go 0))
          nil
          (recur (z/next zip) (if elem (dec to-go) to-go)))))))

(defrecord Node [triple element sub-counter children])

(declare add del fetch index-of indexes)

(defn add
  [{:keys [children triple] :as this}
   {-children :children -element :element -triple :triple
    :as target}]
  (let [this* (update-in this [:sub-counter] inc)
        index (find-in (map :triple children) -triple)
        length (count children)]
    ;does the path exist
    (if (or (< index 0)
            (= length 0)
            (and (= index 0) (> length 0)
                 (not= (:triple (first children)) -triple)))
      (assoc this* :children (->> (cons target children)
                                  (sorted :triple)
                                  (into [])))
      ;does the target have children
      (if (= (count -children) 0)
        ; is their something at the index
        (if (get-in children [index :element])
          this
          (assoc-in this* [:children index :element] -element))
        (update-in this*
                   [:children index]
                   #(add % (first -children)))))))

(defn del
  [this node]
  this)

(defn left-to-right
  [position tree]
  (loop [pos position, branch (-> tree node-zip z/down), acc 0]
    (let [{:keys [sub-counter element]} (z/node branch)
          acc* (if element (inc acc) acc)]
      (if (= pos 0)
        acc*
        (recur (dec pos)
               (z/right branch)
               (+ sub-counter acc*))))))

(defn right-to-left
  [position {:keys [children] :as tree}]
  (loop [pos 0
         branch (-> tree node-zip z/down z/rightmost)
         acc 0]
    (let [{:keys [sub-counter element]} (z/node branch)]
      (if (= (+ pos (inc position)) (count children))
        (- (:sub-counter tree) acc)
        (recur (inc position)
               (z/left branch)
               (+ sub-counter (if element (inc acc) acc)))))))

(defn strat
  [position acc sub-count]
  (if (< (- position acc) (/ sub-count 2))
    left-to-right right-to-left))

(defn index-of
  [this node]
  (if-let [full-path (indexes this node)]
      (loop [path full-path,
             tree this,
             acc (if (:element tree) 1 0)]
        (let [head (first path), tail (next path)
              {:keys [sub-counter children]} tree]
          (if head
            (recur tail
                   (nth children head)
                   (+ acc
                      ;((strat head acc sub-counter)head tree)
                      (left-to-right head tree)))
            (dec acc))))
      nil))

(defn indexes
  [this node]
  (loop [xs [], tree this, path node]
    (let [{:keys [children]} tree
          index (find-in (map :triple children) (:triple path))]
      ;does the tree contain the path
      (if (or (< index 0)
              (and (= index 0) (= (count children) 0)))
        nil
        ;does the path have more children
        (if (or (= (count (:children path)) 0)
                (= (count children) 0))
          (conj xs index)
          (recur (conj xs index)
                 (get-in tree [:children index])
                 (get-in path [:children 0])))))))

(defn fetch
  "this function goes to a node in a zipper then crawls up making
  a replica of the nodes it sees"
  [this index]
  (loop [zip (crawl-to this index)
         build nil]
    (let [{:keys [triple element]} (z/node zip)
          child (if build
                  (->Node triple nil 1 [build])
                  (->Node triple element 0 []))]
      (if (z/path zip)
        (recur (z/up zip) child)
        build))))

(defn node
  [[triple & xs] element]
  (if (= (count xs) 0)
    (Node. triple element 0 [])
    (Node. triple nil 1 [(node xs element)])))

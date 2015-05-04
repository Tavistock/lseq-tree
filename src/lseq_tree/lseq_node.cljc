(ns lseq-tree.lseq-node
  (:require [lseq-tree.triple :as t :refer [sorted]]
            [lseq-tree.util :refer [direction binary-search]]))

(defn native-index-of [xs n]
  (.indexOf xs n))

(defprotocol INode
  (add [this node])
  (del [this node])
  (index-of [this node])
  (indexes [this node])
  (child [this index]))

(defrecord Node [triple element sub-counter children]
  INode
  (add [this other]
    (let [index (native-index-of (map :triple (:children this))
                                 (:triple other))
          next-this (update-in this [:sub-counter] inc)
          {n-children :children n-triple :triple} next-this
          {o-children :children o-element :element o-triple :triple} other
          length (count n-children)]
      (if (or (< index 0)
              (= length 0)
              (and (= index 0)
                   (> length 0)
                   (not= (:triple (first n-children)) o-triple)))
        ; if the path does not exist
        (assoc next-this
               :children
               (into []
                     (sorted :triple (cons other n-children))))
        ; if the path does exist
        (if (= (count o-children) 0)
          ; if the other node does not have children
          (if (not= (get-in n-children [index :element]) nil)
            ; if something is at index
            next-this ; TODO is this a bug in original code
            ; if nothing is at index
            (assoc-in next-this [:children index :element] o-element))
          ; if the other node does have children
          (update-in next-this
                     [:children index]
                     #(add % (first o-children)))))))

  (del [this node] nil)
  (index-of [this node]
    (let [cur-indexes (indexes this node)
          sum (atom 0)
          inner (atom 0)]
      (do
        (when (:element this) (swap! sum inc))
        (if (not cur-indexes)
          nil
          (loop [cur-index (first cur-indexes)
                 cur-rest (rest cur-indexes)
                 cur-tree this]
            (do
              (if (< (- cur-index @sum)
                     (/ (:sub-counter cur-tree) 2))
                (do
                  (reset! inner 0)
                  (while (< @inner cur-index)
                    (let [branch (get-in cur-tree
                                         [:children @inner])]
                      (do
                        (when (:element branch)
                          (swap! sum inc))
                        (swap! sum + (:sub-counter branch))
                        (swap! inner inc)))))
                (do
                  (reset! inner (count (:children cur-tree)))
                  (swap! sum + (:sub-counter cur-tree))
                  (while (>= @inner cur-index)
                    (let [branch (get-in cur-tree
                                         [:children @inner])]
                      (do
                        (when (:element branch)
                          (swap! sum dec))
                        (swap! sum - (:sub-counter branch))
                        (swap! inner dec))))
                  (swap! inner inc)))
              (when (get-in cur-tree
                            [:children @inner :element])
                (swap! sum inc))
              (if (empty? cur-rest)
                (- @sum 1)
                (recur (first cur-rest)
                       (rest cur-rest)
                       (get-in cur-tree
                               [:children @inner])))))))))

  (indexes [this node]
    (loop [xs [], current-tree this, current-node node]
      (let [index (native-index-of
                    (map :triple (:children current-tree))
                    (:triple current-node))]
        (if (or (< index 0)
                (and (= index 0)
                     (= (count (:children current-tree)) 0)))
          nil
          (if (or (= (count (:children current-node)) 0)
                  (= (count (:children current-tree)) 0))
            (conj xs index)
            (recur (conj xs index)
                   (get-in current-tree [:children index])
                   (get-in current-node [:children 0])))))))

  (child [this index] nil))

(defmethod direction Node
  [this that]
  (direction (:triple this) (:triple that)))

(defn node
  [[triple & xs] element]
  (if (= (count xs) 0)
    (Node. triple element 0 [])
    (Node. triple nil 1 [(node xs element)])))

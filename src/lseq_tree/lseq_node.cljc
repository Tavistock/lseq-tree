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
  (fetch [this index]))

(defrecord Node [triple element sub-counter children]
  INode
  (add [{:keys [children triple] :as this}
        {-children :children
         -element :element
         -triple :triple
         :as target}]
    (let [this* (update-in this [:sub-counter] inc)
          index (native-index-of (map :triple children) -triple)
          length (count children)]
      (if (or (< index 0)
              (= length 0)
              (and (= index 0)
                   (> length 0)
                   (not= (:triple (first children)) -triple)))
        ; if the path does not exist
        (assoc this* :children (->> (cons target children)
                                    (sorted :triple)
                                    (into [])))
        ; if the path does exist
        (if (= (count -children) 0)
          ; if the other node does not have children
          (if (get-in children [index :element])
            ; if something is at index
            this*
            ; if nothing is at index
            (assoc-in this* [:children index :element] -element))
          ; if the other node does have children
          (update-in this* [:children index] #(add % (first -children)))))))

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

  (fetch [this index] nil))

(defmethod direction Node
  [this that]
  (direction (:triple this) (:triple that)))

(defn node
  [[triple & xs] element]
  (if (= (count xs) 0)
    (Node. triple element 0 [])
    (Node. triple nil 1 [(node xs element)])))

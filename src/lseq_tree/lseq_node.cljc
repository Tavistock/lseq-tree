(ns lseq-tree.lseq-node
  (:require [lseq-tree.triple :as t]))

(defprotocol INode
  (add [this node])
  (del [this node])
  (direction [this other])
  (index-of [this node])
  (index [this node])
  (child [this index]))

(defrecord Node [triple element sub-counter children]
  INode
  (add [this node] nil)
  (del [this node] nil)
  (direction
    [_ other]
    (t/direction triple (:triple other)))
  (index-of [this node] nil)
  (index [this node] nil)
  (child [this index] nil))

(defn node
  [[triple & xs] element]
  (if (= (count xs) 0)
    (Node. triple element 0 [])
    (Node. triple nil 1 [node xs element])))

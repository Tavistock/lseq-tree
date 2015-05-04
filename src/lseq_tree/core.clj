(ns lseq-tree.core
  (:require [lseq-tree.util :as util]
            [lseq-tree.lseq-node :as n :refer [node add]]
            [lseq-tree.triple :as t :refer [triple]]))

(def node1 (node [(triple 1 2 3)] "a"))
(def node2 (node [(triple 4 5 6)
                  (triple 7 8 9)] "b"))
(def node3 (node [(triple 4 5 6)
                  (triple 10 11 12)] "c"))
(def node4 (reduce add [node1 node2 node3]))
(def fchild (first (:children node4)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

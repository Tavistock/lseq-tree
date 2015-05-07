(ns lseq-tree.core
  (:require [lseq-tree.util :as util]
            [lseq-tree.lseq-node :as n :refer [node add]]
            [lseq-tree.triple :as t :refer [triple]]
            [clojure.zip :as z]))

(def node1 (node [] "a"))
(def node2 (node [(triple 1 1 1)] "b"))
(def node3 (node [(triple 2 2 2)
                  (triple 3 3 3)] "d"))
(def node4 (node [(triple 2 2 2)] "c"))
(def root (reduce add [node1 node2 node3 node4]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

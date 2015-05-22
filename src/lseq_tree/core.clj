(ns lseq-tree.core
  (:require [lseq-tree.lseq-node :as n :refer [node add]]
            [lseq-tree.triple :as t :refer [triple]]
            [lseq-tree.base :as b]
            [lseq-tree.identifier :as i]
            [lseq-tree.strategy :as s]
            [clojure.zip :as z]))

(def start (node [] nil))
(def a (node [(triple 1 1 1)] "a"))
(def b (node [(triple 2 2 2)] "b"))
(def g (node [(triple 3 3 3)] "g"))
(def c (node [(triple 2 2 2)(triple 1 1 1)] "c"))
(def d (node [(triple 2 2 2)(triple 2 2 2)] "d"))
(def e (node [(triple 2 2 2)(triple 3 3 3)] "e"))
(def f (node [(triple 2 2 2)(triple 4 4 4)] "f"))
(def root (reduce add [start a b c d e f g]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

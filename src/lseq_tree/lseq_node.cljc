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

(defrecord Node [triple element sub-counter children])

(defn node
  [[triple & xs] element]
  (if (= (count xs) 0)
    (Node. triple element 0 [])
    (Node. triple nil 1 [(node xs element)])))

(declare add del fetch index-of indexes)

(defn merge-child
  "takes children and merges a limb into it..."
  [children limb]
  (->> children (cons limb) (sorted :triple) (into [])))

(defn triple-in
  "finds the triple in a node and returns it's index"
  [{:keys [children] :as node} triple]
  (find-in (map :triple children) triple))

(defn add
  "adds a node with one child at each level(a limb) to the children of a node"
  ; TODO use a indexes func on a limb instead of this
  [{:keys [children triple] :as node}
   {-children :children -element :element -triple :triple
    :as limb}]
  (let [node* (update-in node [:sub-counter] inc)
        index (triple-in node* -triple)
        length (count children)]
    ;does the path exist
    (if (or (< index 0)
            (= length 0))
      (assoc node* :children (merge-child children limb))
      ;does the limb have children
      (if (= (count -children) 0)
        ; is their something at the index
        (if (get-in children [index :element])
          node
          (assoc-in node* [:children index :element] -element))
        (update-in node* [:children index] #(add % (first -children)))))))

(defn del
  [this node]
  this)

(defn left-to-right
  "a strategy that returns the sum of the branches to the left of a position
  in a node"
  [position node]
  (loop [pos position, branch (-> node node-zip z/down), acc 0]
    (let [{:keys [sub-counter element]} (z/node branch)
          acc* (if element (inc acc) acc)]
      (if (= pos 0)
        acc*
        (recur (dec pos)
               (z/right branch)
               (+ sub-counter acc*))))))

(defn index-of
  "returns the zero-based index of a limb in a node"
  [node limb]
  (let [paths (indexes node limb)]
    (if (some nil? paths)
      nil
      (loop [paths paths,
             node node,
             acc (if (:element node) 1 0)]
        (let [head (first paths), tail (next paths)
              {:keys [children]} node]
          (if head
            (recur tail
                   (nth children head)
                   (+ acc (left-to-right head node)))
            (dec acc))))))) ;zero-based index

(defn indexes
  "returns a vector of numbers for each level that the index of a limb is
  located at in a node. returns nil instead of a number when it can't find the
  limb (eg [1 2 3 nil])"
  [node limb]
  (loop [path [], node node, limb limb]
    (let [{:keys [children]} node
          index (find-in (map :triple children) (:triple limb))]
      ;does the node contain the limb
      (if (or (< index 0)
              (and (= index 0) (= (count children) 0)))
        (conj path nil)
        ;does the limb have more children
        (if (or (= (count (:children limb)) 0)
                (= (count children) 0))
          (conj path index)
          (recur (conj path index)
                 (get-in node [:children index])
                 (get-in limb [:children 0])))))))

(defn crawl-to
  "returns a zipper with it's location where the index in a node"
  [node index]
  (loop [zip (node-zip node)
         to-go index]
    (let [elem (:element (z/node zip))]
      (if (and (= to-go 0) elem)
        zip
        (if (and (z/end? zip) (>= to-go 0))
          nil
          (recur (z/next zip) (if elem (dec to-go) to-go)))))))

(defn crawl-out
  "this crawls up making from child node in a zipper making a replica of the
  nodes it sees"
  [child]
  (loop [zip child
         build nil]
    (let [{:keys [triple element]} (z/node zip)
          child (if build
                  (->Node triple nil 1 [build])
                  (->Node triple element 0 []))]
      (if (z/path zip)
        (recur (z/up zip) child)
        build))))

(defn fetch [node index]
  "takes a node and an index and returns a recreation of the node at that index
  as a limb"
  (-> node
      (crawl-to index)
      (crawl-out)))


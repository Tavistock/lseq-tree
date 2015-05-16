(ns lseq-tree.lseq-node
  (:require [lseq-tree.triple :as t :refer [sorted]]
            [clojure.zip :as z]))

(defn find-in [xs n]
  (.indexOf xs n))

(defrecord Node [triple element sub-counter children])

(defn count-elements
  [{:keys [sub-counter element]}]
  (if element
    (inc sub-counter)
    sub-counter))

(defn count-children
  [children]
  (reduce + (map count-elements children)))

(defn node-zip
  [root]
  (z/zipper
    (fn [node] (:children node))
    (fn [node] (seq (:children node)))
    (fn [{:keys [triple element]} children]
      (->Node triple
              element
              (count-children children)
              (sorted :triple children)))
    root))

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

(defn nth-child
  "Returns the loc of the nth child of the node at this loc, or
  nil if no children or nth-child"
  [loc n]
  (when (z/branch? loc)
    (let [[node path] loc
          cs (z/children loc)
          c (nth cs n nil)]
      (when (and cs c)
        (with-meta [c
                    {:l (if (> n 0) (vec (take n cs)) [])
                     :pnodes (if path
                               (conj (:pnodes path) node)
                               [node])
                     :ppath path
                     :r (drop (+ n 1) cs)}]
                   (meta loc))))))

(defn add
  "adds a node with one child at each level(a limb) to the children of a node"
  ; TODO use a indexes func on a limb instead of this
  [node limb]
  (let [paths (indexes node limb)]
    (if (last paths)
      (loop [[head & tail] paths,
             node-loc (node-zip node),
             limb limb]
        (let [node-child (nth-child node-loc head)]
          (if (empty? tail)
            (if (-> node-child z/node :element)
              node
              (-> node-child
                  (z/edit assoc :element (:element limb))
                  z/root))
            (recur tail
                   node-child
                   (first (:children limb))))))
      (loop [[head & tail] paths
             node-loc (node-zip node)
             limb limb]
        (if-not head
          (-> node-loc (z/append-child limb) z/root)
          (recur tail
                 (nth-child node-loc head)
                 (first (:children limb))))))))

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
      (loop [[head & tail] paths,
             {:keys [children] :as node} node,
             acc (if (:element node) 1 0)]
        (if head
          (recur tail
                 (nth children head)
                 (+ acc (left-to-right head node)))
          (dec acc)))))) ;zero-based index

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
                 (nth children index)
                 (first (:children limb))))))))

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


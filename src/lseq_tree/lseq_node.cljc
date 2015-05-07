(ns lseq-tree.lseq-node
  (:require [lseq-tree.triple :as t :refer [sorted]]
            [clojure.zip :as z]))

(defn find-in [xs n]
  (.indexOf xs n))

(defn node-zip
  [root]
  (z/zipper
    (fn [_] true)
    (fn [n] (:children n))
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

(defprotocol INode
  (add [this node])
  (del [this node])
  (index-of [this node])
  (indexes [this node])
  (fetch [this index]))

(defrecord Node [triple element sub-counter children]
  INode
  (add [{:keys [children triple] :as this}
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
            this*
            (assoc-in this* [:children index :element] -element))
          (update-in this*
                     [:children index]
                     #(add % (first -children)))))))

  (del [this node] this)
  (index-of [this node]
    ;TODO make this not so horrible
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
    (loop [xs [],
           {:keys [children] :as tree} this,
           path node]
      (let [index (find-in (map :triple children) (:triple path))]
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

  (fetch [this index]
    "this function goes to a node in a zipper then crawls up making
    a replica of the nodes it sees"
    (loop [zip (crawl-to this index)
           build nil]
      (let [{:keys [triple element]} (z/node zip)
            child (if build
                    (->Node triple nil 1 [build])
                    (->Node triple element 0 []))]
        (if (z/path zip)
          (recur (z/up zip) child)
          build)))))

(defn node
  [[triple & xs] element]
  (if (= (count xs) 0)
    (Node. triple element 0 [])
    (Node. triple nil 1 [(node xs element)])))

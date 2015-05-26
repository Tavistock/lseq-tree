(ns lseq-tree.lseq-tree
  (:require
    [lseq-tree.base :as b]
    [lseq-tree.identifier :as i]
    [lseq-tree.strategy :as s]
    [lseq-tree.triple :as t]
    [lseq-tree.lseq-node :as n]
    [lseq-tree.util :refer [pow]]))

(def max-value
  #?(:clj Long/MAX_VALUE
          :cljs Number/MAX_VALUE ; 2^53 - 1
          ))

(defrecord Lseq [site counter root strategy base])

(declare fetch length insert delete apply-insert apply-delete)

(defn lseq
  ([site] (lseq site {:base 6 :strategy 10}))
  ([site {:keys [base strategy] :as options}]
   (let [base1 (b/base base)
         root (-> (n/node [] nil)
                  (n/add (n/node [(t/triple 0 0 0)] :start))
                  (n/add (n/node [(t/triple
                                    (b/depth-max base1 0)
                                    max-value
                                    max-value)] :end)))
         strategy1 (s/strategy strategy)]
     (->Lseq site 0 root strategy1 base1))))

(defn level-hash
  [level]
  (mod level 2))

(defn length [tree]
  (-> tree :root :sub-counter (- 2)))

(defn fetch
  [{:keys [root]} index]
  (n/fetch root index))

(declare alloc)

(defn insert
  [tree element index]
  (let [lower (fetch tree index)
        upper (fetch tree (inc index))
        tree1 (update-in tree [:counter] inc)
        id (alloc tree1 [lower upper])
        tree2 (apply-insert tree1 element id)]
    [tree2 [element id]]))

(defn delete
  [{:keys [base root] :as tree} index]
  (let [node (n/fetch root (inc index))
        id (i/node->id node base)
        tree1 (apply-delete tree id)]
    [tree1 id]))

(defn alloc
  [{:keys [strategy site counter base]} pair]
  (let [{:keys [level] :as extra} (s/extra-data pair base)
        partial-id (i/id nil site counter)]
    (do (prn extra)
     (if true
     ; (= (level-hash level) 0)
      (s/b+ strategy pair partial-id extra base)
      (s/b- strategy pair partial-id extra base)))))

(defn debug-alloc
  [{:keys [strategy site counter base] :as tree} n sign]
  (let [pair [(fetch tree n) (fetch tree (inc n))]
        {:keys [level] :as extra} (s/extra-data pair base)
        partial-id (i/id nil site counter)]
    (if (= :+ sign)
      (s/b+ strategy pair partial-id extra base)
      (s/b- strategy pair partial-id extra base))))

(defn apply-insert
  [{:keys [base] :as tree} element id]
  (let [node (i/id->node id element base)]
    (update-in tree [:root] #(n/add % node))))

(defn apply-delete
  [{:keys [base] :as tree} id]
  (let [node (i/id->node id nil base)]
    (update-in tree [:root] #(n/del % node))))

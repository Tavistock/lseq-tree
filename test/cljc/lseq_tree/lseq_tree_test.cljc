(ns lseq-tree.lseq-tree-test
  (:require #?(:clj [clojure.test :refer  [are is testing deftest]]
                    :cljs [cljs.test :refer-macros  [are is testing deftest]])
            [lseq-tree.lseq-tree :refer :all]
            [lseq-tree.strategy :as s]
            [lseq-tree.identifier :as i]
            [lseq-tree.lseq-node :as n]
            [lseq-tree.triple :as t]
            [lseq-tree.base :as b]))

(def ^:dynamic *runs* 100)

(deftest tree-construct
  (testing "make a basic tree"
    (let [tree (lseq 42)]
      (is (= (:site tree) 42))
      (is (= (:counter tree) 0))
      (is (= (length tree) 0)))))

(deftest insert-test
  (testing "insert an element into the structure"
    (let [tree (lseq 42)
          [tree1 ei] (insert tree "a" 0)]
      (is (= (:counter tree1) 1))
      (is (= (:site tree1) 42))
      (is (= (:element (fetch tree1 1)) "a"))
      (is (= (length tree1) 1))))
  (testing (str "insert " *runs*
                " element(s) into the structure at beginning")
    (loop [n 0 tree (lseq 42)]
      (if (= n *runs*)
        (is (= (length tree) *runs*))
        (recur (inc n) (first (insert tree "a" 0))))))
  (testing (str "insert " *runs*
                " element(s) into structure at end")
    (loop [n 0 tree (lseq 42)]
      (if (= n *runs*)
        (is (= (length tree) *runs*))
        (recur (inc n) (first (insert tree "a" n))))))
  (testing (str "insert " *runs*
                " element(s) into structure at random position")
    (loop [n 0 tree (lseq 42)]
      (if (= n *runs*)
        (is (= (length tree) *runs*))
        (recur (inc n)
               (first (insert tree "a" (int (rand n)))))))))

(deftest delete-test
  (testing "insert and delete an element from a structure"
    (let [tree (lseq 42)
          [tree1 ei1] (insert tree "a" 0)
          [tree2 id1] (delete tree1 0)]
      (is (= (:counter tree1) 1))
      (is (= (:site tree1) 42))
      (is (= (:element (fetch tree1 1)) "a"))
      (is (= (length tree1) 1))
      (is (= (:element (fetch tree2 1)) :end))
      (is (= (length tree2) 0))))
  (testing "insert and delete " *runs* "elements to structure"
    (let [tree (lseq 42)
          tree1 (loop [n 0 tree tree]
                  (if (= n *runs*)
                    tree
                    (recur (inc n)
                           (first (insert tree "a"
                                          (int (rand n)))))))
          tree2 (loop [n (dec *runs*) tree tree1]
                      (if (< n 0)
                        tree
                        (recur (dec n)
                               (first
                                 (delete tree (int (rand n)))))))]
      (is (= (length tree1 *runs*)))
      (is (= (length tree2 0))))))

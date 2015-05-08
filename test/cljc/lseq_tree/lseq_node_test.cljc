(ns lseq-tree.lseq-node-test
  (:require #?(:clj [clojure.test :refer [are is testing deftest]]
               :cljs [cljs.test :refer-macros [are is testing deftest]])
            [lseq-tree.triple :refer [triple]]
            [lseq-tree.lseq-node :refer :all]))

(deftest construction
  (testing "making a node of length 1"
    (let [node1 (node [(triple 1 2 3)] "a")]
      (is (= (:sub-counter node1 0)))
      (is (= (:element node1) "a"))
      (is (= (count (:children node1)) 0))))

  (testing "making a node of length 3"
    (let [node1 (node [(triple 1 2 3)
                       (triple 4 5 6)
                       (triple 7 8 9)] "a")
          fchild (first (:children node1))
          ffchild (first (:children fchild))]
      (is (= (:sub-counter node1) 1))
      (is (= (:sub-counter fchild) 1))
      (is (= (:sub-counter ffchild) 0))
      (is (= (:element ffchild) "a"))
      (is (= (count (:children node1)) 1)))))

(deftest test-add
  (testing "create a tree of 2 elements (1 subpath)"
    (let [node1 (node [(triple 1 2 3)] "a")
          node2 (node [(triple 4 5 6)
                       (triple 7 8 9)] "b")
          node3 (add node1 node2)]
      (is (= (:sub-counter node3) 1))))

  (testing "construct a tree of 2 elements (2 subpaths)"
    (let [node1 (node [(triple 1 2 3)] "a")
          node2 (node [(triple 4 5 6)
                       (triple 7 8 9)] "b")
          node3 (node [(triple 4 5 6)
                       (triple 10 11 12)] "c")
          node4 (reduce add [node1 node2 node3])
          fchild (first (:children node4))]
      (is (= (:sub-counter node4) 2))
      (is (= (count (:children node4)) 1))
      (is (= (count (:children fchild)) 2))))

  (testing "construct a tree, verify index locations"
    (let [a (node [] "a")
          b (node [(triple 1 1 1)] "b")
          c (node [(triple 2 2 2)
                       (triple 3 3 3)] "c")
          d (node [(triple 2 2 2)
                       (triple 4 4 4)] "d")
          root (reduce add [a b c d])]
      (is (= (:sub-counter root) 3))
      (is (= (index-of root b) 1))
      (is (= (index-of root c) 2))
      (is (= (index-of root d) 3))))

  (testing "construct a tree, insert element into existing branch"
    (let [node1 (node [] "a")
          node2 (node [(triple 1 1 1)
                       (triple 3 3 3)] "c")
          node3 (node [(triple 1 1 1)] "b")
          node4 (reduce add [node1 node2 node3])]
      (is (= (:sub-counter node4) 2))
      (is (= (index-of node4 node2) 2))
      (is (= (index-of node4 node3) 1))))
  (testing "longer tree"
    (let [start (node [] nil)
          a (node [(triple 1 1 1)] "a")
          b (node [(triple 2 2 2)] "b")
          g (node [(triple 3 3 3)] "g")
          c (node [(triple 2 2 2)(triple 1 1 1)] "c")
          d (node [(triple 2 2 2)(triple 2 2 2)] "d")
          e (node [(triple 2 2 2)(triple 3 3 3)] "e")
          f (node [(triple 2 2 2)(triple 4 4 4)] "f")
          root (reduce add [start a b c d e f g])]
      (are [x y] (= (index-of root x) y)
           a 0 b 1 c 2 d 3 e 4 f 5 g 6))))

(deftest test-fetch
  (testing "fetch value at index for simple tree"
    (let [node1 (node [] "a")
          node2 (node [(triple 1 1 1)] "b")
          node3 (node [(triple 2 2 2)] "c")
          root (reduce add [node1 node2 node3])]
      (is (= (:sub-counter root) 2))
      (is (= (fetch root 1) node2))
      (is (= (fetch root 2) node3))))
  (testing "fetch value at index of depth 2 tree"
    (let [node1 (node [] "a")
          node2 (node [(triple 1 1 1)] "b")
          node3 (node [(triple 2 2 2)
                       (triple 3 3 3)] "d")
          node4 (node [(triple 2 2 2)] "c")
          root (reduce add [node1 node2 node3 node4])]
      (is (= (fetch root 1) node2))
      (is (= (fetch root 2) node4))
      (is (= (fetch root 3) node3)))))

#_(deftest test-del
  (testing "should delete a node of a siple tree"
    (let [node1 (node [] "a")
          node2 (node [(triple 1 1 1)] "b")
          node3 (node [(triple 2 2 2)] "c")
          root (reduce add [node1 node2 node3])
          del-root (del root node2)]
      (is (= (index-of root node3) 2))
      (is (= (index-of del-root node3) 1))))
  (testing "should delete all intermediate nodes on a path"
    (let [node1 (node [] "a")
          node2 (node [(triple 1 1 1)
                       (triple 2 2 2)
                       (triple 3 3 3)] "b")
          node3 (node [(triple 1 1 1)] "c")
          root (reduce add [node1 node2 node3])
          del-root (del root node2)
          del-fchild (first (:children del-root))]
      (is (= (index-of root node3) 1))
      (is (= (index-of root node2) 2))
      (is (= (index-of del-root node2) 1))
      (is (= (:element del-fchild) "c"))
      (is (= (count (:children del-fchild)) 0))))
  (testing "should not delete intermediate nodes in complex tree"
    (let [node1 (node [] "a")
          node2 (node [(triple 1 1 1)
                       (triple 2 2 2)
                       (triple 3 3 3)] "b")
          node3 (node [(triple 1 1 1)] "c")
          node4 (node [(triple 1 1 1)
                       (triple 2 2 2)
                       (triple 4 4 4)
                       (triple 5 5 5)] "d")
          root (reduce add [node1 node2 node3 node4])
          root2 (del root node2)
          root3 (del root2 node3)]
      (is (= (index-of root node3) 1))
      (is (= (index-of root node2) 2))
      (is (= (index-of root node4) 3))
      (is (= (index-of root2 node3) 1))
      (is (= (index-of root2 node4) 2))
      (is (= (index-of root3 node4) 1)))))

(def test-indexes
  (testing "gets a vector of the path in the tree for a node"
    (let [node1 (node [] nil)
          node2 (node [(triple 1 1 1)] "a")
          node3 (node [(triple 2 2 2)
                       (triple 3 3 3)] "b")
          node4 (node [(triple 2 2 2)
                       (triple 4 4 4)] "c")
          root (reduce add [node1 node2 node3 node4])]
      (is (= (:sub-counter root) 3))
      (is (= (index-of root node2) 0))
      (is (= (index-of root node3) 1))
      (is (= (index-of root node4) 2))
      (is (= (indexes root node2) [0]))
      (is (= (indexes root node3) [1 0]))
      (is (= (indexes root node4) [1 1])))))

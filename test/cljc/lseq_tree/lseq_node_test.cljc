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
    (let [node1 (node [] "a")
          node2 (node [(triple 1 1 1)] "b")
          node3 (node [(triple 2 2 2)
                       (triple 3 3 3)] "c")
          node4 (node [(triple 2 2 2)
                       (triple 4 4 4)] "d")
          node5 (reduce add [node1 node2 node3 node4])]
      (is (= (:sub-counter node5) 3))
      (is (= (index-of node5 node2) 1))
      (is (= (index-of node5 node3) 2))
      (is (= (index-of node5 node4) 3))))

  (testing "construct a tree, insert element into existing branch"
    (let [node1 (node [] "a")
          node2 (node [(triple 1 1 1)
                       (triple 3 3 3)] "c")
          node3 (node [(triple 1 1 1)] "b")
          node4 (reduce add [node1 node2 node3])]
      (is (= (:sub-counter node4) 2))
      (is (= (index-of node4 node2) 2))
      (is (= (index-of node4 node3) 1)))))


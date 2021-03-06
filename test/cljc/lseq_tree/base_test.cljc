(ns lseq-tree.base-test
  (:require #?(:clj [clojure.test :refer  [are is testing deftest]]
                    :cljs [cljs.test :refer-macros  [are is testing deftest]])
            [lseq-tree.base :refer :all]
            [lseq-tree.lseq-node :as n]
            [lseq-tree.triple :as t]))

(deftest setup
  (testing "trivial setup"
    (is (= (:size (base 43)) 43))))

(deftest singleton
  (testing "base is not a singleton"
    (let [base1 (base 42)
          base2 (base)]
      (is (= (:size base1) 42)))))

(deftest bit-base
  (testing "get the bit for a base at a certain level"
    (is (= (bit (base 42) 5) 47))))

(deftest sum-base
  (testing "return the sum of bits from level x to level 0"
    (are [x y] (= (sum (base 5) x) y)
         0 5
         1 11 ; (5*2)+1
         2 18))) ; (5*3)+3

(deftest interval-base
  (let [node1 (n/node [(t/triple 1 0 0)
                       (t/triple 1 0 0)] nil)
        node2 (n/node [(t/triple 1 0 0)] nil)
        node3 (n/node [(t/triple 1 0 0)
                       (t/triple 15 0 0)] nil)
        node4 (n/node [(t/triple 1 0 0)
                       (t/triple 4 0 0)] nil)
        node5 (n/node [(t/triple 1 0 0)
                       (t/triple 3 0 0)] nil)
        node6 (n/node [(t/triple 2 0 0)] nil)
        base1 (base 3)]
    (testing "returns a 0 at level 0"
      (is (== (interval base1 node1 node3 0) 0)))
    (testing "should return an interval at level 1 of 13"
      (is (== (interval base1 node1 node3 1) 13)))
    (testing "should return an interval at level 1 of 14"
      (is (== (interval base1 node2 node3 1) 14)))
    (testing "should return an interval at level 1 of 11"
      (is (== (interval base1 node4 node5 1) 11)))
    (testing "process interval between close paths with node at 0"
      (is (== (interval base1 node2 node6 0) 0)))
    (testing "process interval between close paths with node at 1"
      (is (== (interval base1 node2 node6 1) 15)))))


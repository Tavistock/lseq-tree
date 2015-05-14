(ns lseq-tree.lseq-node-test
  (:require #?(:clj [clojure.test :refer [are is testing deftest]]
               :cljs [cljs.test :refer-macros [are is testing deftest]])
            [lseq-tree.triple :refer [triple]]
            [lseq-tree.lseq-node :refer :all]))

; helpers
(defn t [n] (triple n n n))

(deftest construction
  (testing "making a node of length 1"
    (let [node-a (node [(t 1)] "a")]
      (is (= (:sub-counter node-a 0)))
      (is (= (:element node-a) "a"))
      (is (= (count (:children node-a)) 0))))
  (testing "making a node of length 3"
    (let [node-a (node [(t 1)(t 2)(t 3)] "a")
          fchild (first (:children node-a))
          ffchild (first (:children fchild))]
      (is (= (:sub-counter node-a) 1))
      (is (= (:sub-counter fchild) 1))
      (is (= (:sub-counter ffchild) 0))
      (is (= (:element ffchild) "a"))
      (is (= (count (:children node-a)) 1)))))

(deftest test-add
  (let [start (node [] nil)
        a (node [(t 1)] "a")
        b (node [(t 2)] "b")
        c (node [(t 2)(t 1)] "c")
        d (node [(t 2)(t 2)] "d")
        e (node [(t 2)(t 3)] "e")
        f (node [(t 2)(t 4)] "f")
        g (node [(t 3)] "g")]
    (testing "create a tree of 2 elements (1 subpath)"
      (let [root (add start a)]
        (is (= (:sub-counter root) 1))))
    (testing "construct a tree of 2 elements (2 subpaths)"
      (let [root (reduce add [start c d])
            fchild (first (:children root))]
        (is (= (:sub-counter root) 2))
        (is (= (count (:children root)) 1))
        (is (= (count (:children fchild)) 2))))
    (testing "construct a tree, verify index locations"
      (let [root (reduce add [start b c d])]
        (is (= (:sub-counter root) 3))
        (is (= (index-of root b) 0))
        (is (= (index-of root c) 1))
        (is (= (index-of root d) 2))))
    (testing "construct a tree, insert element into existing branch"
      (let [root (reduce add [start c b])]
        (is (= (:sub-counter root) 2))
        (is (= (index-of root c) 1))
        (is (= (index-of root b) 0))))
    (testing "makes a large tree, it tests idempotence as well as abitrary
             ordering (because 'add' is logically monotonic)"
      (let [root (reduce add start
                         (shuffle (take 14 (cycle [a b c d e f g]))))]
        (are [x y] (= (index-of root x) y)
             a 0 b 1 c 2 d 3 e 4 f 5 g 6)
        (is (= (:sub-counter root) 7))))))

(deftest test-fetch
  (testing "fetch value at index for simple tree"
    (let [a (node [] "a")
          b (node [(triple 1 1 1)] "b")
          c (node [(triple 2 2 2)] "c")
          root (reduce add [a b c])]
      (is (= (:sub-counter root) 2))
      (is (= (fetch root 1) b))
      (is (= (fetch root 2) c))))
  (testing "fetch value at index of depth 2 tree"
    (let [a (node [] "a")
          b (node [(t 1)] "b")
          c (node [(t 2)] "c")
          d (node [(t 2)(t 3)] "d")
          root (reduce add [a b d c])]
      (is (= (fetch root 1) b))
      (is (= (fetch root 2) c))
      (is (= (fetch root 3) d)))))

#_(deftest test-del
    (testing "should delete a node of a siple tree"
      (let [a (node [] "a")
            b (node [(triple 1 1 1)] "b")
            c (node [(triple 2 2 2)] "c")
            root (reduce add [a b c])
            del-root (del root b)]
        (is (= (index-of root c) 2))
        (is (= (index-of del-root c) 1))))
    (testing "should delete all intermediate nodes on a path"
      (let [a (node [] "a")
            b (node [(t 1)(t 2)(t 3)] "b")
            c (node [(t 1)] "c")
            root (reduce add [a b c])
            del-root (del root b)
            del-fchild (first (:children del-root))]
        (is (= (index-of root c) 1))
        (is (= (index-of root b) 2))
        (is (= (index-of del-root b) 1))
        (is (= (:element del-fchild) "c"))
        (is (= (count (:children del-fchild)) 0))))
    (testing "should not delete intermediate nodes in complex tree"
      (let [a (node [] "a")
            b (node [(t 1)(t 2)(t 3)] "b")
            c (node [(t 1)] "c")
            d (node [(t 1)(t 2)(t 4)(t 5)] "d")
            root (reduce add [a b c d])
            root2 (del root b)
            root3 (del root2 c)]
        (is (= (index-of root c) 1))
        (is (= (index-of root b) 2))
        (is (= (index-of root d) 3))
        (is (= (index-of root2 c) 1))
        (is (= (index-of root2 d) 2))
        (is (= (index-of root3 d) 1)))))

(def test-indexes
  (testing "gets a vector of the path in the tree for a node"
    (let [start (node [] nil)
          a (node [(t 1)] "a")
          b (node [(t 2)(t 3)] "b")
          c (node [(t 2)(t 4)] "c")
          root (reduce add [start a b c])]
      (is (= (:sub-counter root) 3))
      (is (= (index-of root a) 0))
      (is (= (index-of root b) 1))
      (is (= (index-of root c) 2))
      (is (= (indexes root a) [0]))
      (is (= (indexes root b) [1 0]))
      (is (= (indexes root c) [1 1])))))

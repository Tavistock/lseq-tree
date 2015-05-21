(ns lseq-tree.identifier-test
  (:refer-clojure :exclude [compare])
  (:require #?(:clj [clojure.test :refer  [are is testing deftest]]
                    :cljs [cljs.test :refer-macros  [are is testing deftest]])
            [lseq-tree.identifier :refer :all]
            [lseq-tree.base :as b]
            [lseq-tree.lseq-node :as n]
            [lseq-tree.triple :as t]))

(deftest full-test
  (let [base1 (b/base 3)]
    (testing "id is initialized correctly"
      (let [{:keys [digit site counter]} (id 0 [0] [0])]
        (is (= digit 0))
        (is (= (count site) 1))
        (is (= (count counter) 1))))
    (testing "compare:"
      (testing "level 0 works"
        (let [id1 (id 3 [0] [0])
              id2 (id 5 [0] [0])]
          (is (< (compare id1 id2) 0))
          (is (> (compare id2 id1) 0))))
      (testing "level 1 works"
        (let [id1 (id 19 [0 1] [0 1]) ;; d: [1 3]
              id2 (id 37 [0 1] [0 1])] ;; d: [2 5]
          (is (< (compare id1 id2) 0))
          (is (> (compare id2 id1) 0))))
      (testing "level 1 where level 0 is equal works"
        (let [id1 (id 19 [0 1] [0 1]) ;; d: [1 3]
              id2 (id 21 [0 1] [0 1])] ;; d: [1 5]
          (is (< (compare id1 id2) 0))
          (is (> (compare id2 id1) 0))))
      (testing "level 1 with different source works"
        (let [id1 (id 19 [0 3] [0 2]) ;; d: [1 3]
              id2 (id 21 [3 3] [0 2])] ;; d: [1 5]
          (is (< (compare id1 id2) 0))
          (is (> (compare id2 id1) 0))))
      (testing "level 1 with different clock works"
        (let [id1 (id 19 [3 3] [0 1]) ;; d: [1 3]
              id2 (id 21 [3 3] [0 2])] ;; d: [1 5]
          (is (< (compare id1 id2) 0))
          (is (> (compare id2 id1) 0))))
      (testing "different sized paths works"
        (let [id1 (id 1 [3] [0]) ;; d: [1]
              id2 (id 21 [3 3] [0 2])] ;; d: [1 5]
          (is (< (compare id1 id2) 0))
          (is (> (compare id2 id1) 0)))))
    (testing "level 1 with equal ids works"
        (let [id1 (id 21 [3 3] [0 2]) ;; d: [1 5]
              id2 (id 21 [3 3] [0 2])] ;; d: [1 5]
          (is (= (compare id1 id2) 0))
          (is (= (compare id2 id1) 0))))

    (let [node-lit
          (n/node [(t/triple 1 3 0) (t/triple 5 3 2)] nil)
          {:keys [digit site counter] :as id-test}
          (node->id node-lit)

          id-lit (id 21 [3 3] [0 2])
          node-test (id->node id-lit)

          node-convert (-> node-lit node->id id->node)
          id-convert (-> id-lit id->node node->id)]
      (testing "node to id works"
        (is (= digit 21))
        (is (= site [3 3]))
        (is (= counter [0 2])))
      (testing "id to node works"
        (is (= (:triple node-test) (t/triple 1 3 0)))
        (is (= (:triple (first (:children node-test)))
               (t/triple 5 3 2))))
      (testing "convert to alt then back"
        (is (= node-lit node-convert))
        (is (= id-lit id-convert))))))

(ns lseq-tree.identifier-test
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

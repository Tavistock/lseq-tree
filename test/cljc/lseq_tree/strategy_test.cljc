(ns lseq-tree.strategy-test
  (:require #?(:clj [clojure.test :refer  [are is testing deftest]]
                    :cljs [cljs.test :refer-macros  [are is testing deftest]])
            [lseq-tree.strategy :refer :all]
            [lseq-tree.identifier :as i]
            [lseq-tree.lseq-node :as n]
            [lseq-tree.triple :as t]
            [lseq-tree.base :as b]))

(deftest b+digit-test
  (testing "within limits"
    (doall
      (for [x (range 100)]
        (let [b+1 (b+digit
                    10
                    (n/node  [(t/triple 2 3 4)
                              (t/triple 5 6 7)
                              (t/triple 1 2 2)] nil) ; digit = 1185
                    2 (b/base))]
          (is (> b+1 1185))
          (is (< b+1 1196)))))))

(deftest candidate->id-test
  (testing "traverses prev-node and adds its own site and counter"
    (let [id1 (candidate->id
                (i/id 21 9 9) ; path = [1 5]
                [(n/node [(t/triple 1 2 3)
                          (t/triple 4 6 7)] nil) ; take
                (n/node [(t/triple 2 0 0)] nil)] ; pass
                {:level 1}
                (b/base))
          id2 (i/id 21 [2 9] [3 9])]
      (is (= id1 id2))))
  (testing "much mode difficult: on next-node with a short
           prev-node and a next-node thats longer than depth"
    (let [id1 (candidate->id
                (i/id 647 12 13); path = [1 4 7]
                [(n/node [(t/triple 1 1 1)
                         (t/triple 2 2 2)] nil) ; pass this
                (n/node [(t/triple 1 2 3)
                         (t/triple 4 5 6)
                         (t/triple 9 10 11)] nil)]
                {:level 2}
                (b/base))
          id2 (i/id 647 [2 5 12] [3 6 13])]
      (is (= id1 id2)))))

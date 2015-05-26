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
        (let [pair [(n/node  [(t/triple 2 3 4) ; digit @2 = 1185
                              (t/triple 5 6 7)
                              (t/triple 1 2 2)] nil) nil]
              b+1 (b+digit {:boundary 10} pair
                    {:interval 10 :level 2} (b/base))]
          (is (> b+1 1185))
          (is (< b+1 1196)))))))

(deftest bounds-test
  (testing "does upper-paths work"
    (let [node-lower (n/node [(t/triple 2 1 1)
                           (t/triple 5 1 1)
                           (t/triple 1 1 1)] nil)
          path1 (upper-path
                 [node-lower
                  (n/node  [(t/triple 2 1 1)
                            (t/triple 5 1 1)] nil)])
          path2 (upper-path
                  [node-lower
                   (n/node [(t/triple 2 1 1)
                            (t/triple 5 1 1)
                            (t/triple 5 1 1)] nil)])
          path3 (upper-path
                  [node-lower
                   (n/node [(t/triple 2 1 1)] nil)])]
      (is (= [2 5 :max] path1))
      (is (= [2 5 4] path2))
      (is (= [2 :max :max] path3))))
  (testing "does it expand paths correctly"
    (let [bound1 (upper-bound [2 5 :max] (b/base 3))
          bound2 (upper-bound [2 5 4] (b/base 3))
          bound3 (upper-bound [2 :max :max] (b/base 3))]
      (is (= bound1 1215))
      (is (= bound2 1188))
      (is (= bound3 1535)))))

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

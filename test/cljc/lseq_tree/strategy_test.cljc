(ns lseq-tree.strategy-test
  (:require #?(:clj [clojure.test :refer  [are is testing deftest]]
                    :cljs [cljs.test :refer-macros  [are is testing deftest]])
            [lseq-tree.strategy :refer :all]
            [lseq-tree.identifier :as i]
            [lseq-tree.lseq-node :as n]
            [lseq-tree.triple :as t]))

(deftest next-id-test
  (testing "does it work"
    (let [id1 (next-id 21
                       (n/node [(t/triple 1 2 3)
                                (t/triple 4 6 7)] nil)
                       (n/node [(t/triple 2 0 0)] nil)
                       1 9 9)
          id2 (i/id 21 [2 9] [3 9])]
      (is (= id1 id2)))))

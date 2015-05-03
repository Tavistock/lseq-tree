(ns lseq-tree.base-test
  (:require #?(:clj [clojure.test :refer  [is testing deftest]]
               :cljs [cljs.test :refer-macros  [is testing deftest]])
            [lseq-tree.base :refer :all]))

(deftest setup
  (testing "trivial setup."
    (is (:base (new-base 43)) 43)))

(deftest singleton
  (testing "base is not a singleton"
    (let [base1 (new-base 42)
          base2 (new-base)]
      (is (:base base1) 42))))


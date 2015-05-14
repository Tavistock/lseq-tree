(ns lseq-tree.base-test
  (:require #?(:clj [clojure.test :refer  [are is testing deftest]]
               :cljs [cljs.test :refer-macros  [are is testing deftest]])
            [lseq-tree.base :refer :all]))

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

(deftest interval
  (testing "returns an 0 at level 0"
    (is (= 0 1))))


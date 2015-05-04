(ns lseq-tree.triple-test
  (:require #?(:clj [clojure.test :refer  [are is testing deftest]]
               :cljs [cljs.test :refer-macros  [are is testing deftest]])
            [lseq-tree.triple :refer :all]))

(deftest general-direction
  (testing "direction should prefer in oreder: path, site, counter"
    (let [t1 (triple 1 1 1)
          t2 (triple 0 1 1)
          t3 (triple 0 0 1)
          t4 (triple)]
      (are [x y result] (= (sorted [x y]) result)
           t1 t2 (list t2 t1)
           t2 t1 (list t2 t1)
           t2 t3 (list t3 t2)
           t3 t2 (list t3 t2)
           t3 t4 (list t4 t3)
           t4 t3 (list t4 t3)))))

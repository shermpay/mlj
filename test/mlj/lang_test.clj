(ns mlj.lang-test
  (:refer-clojure :exclude [val if fn let])
  (:require [clojure.test :refer :all]
            [mlj.lang :as ml]))

(deftest fn-test
  (testing "ml/fn"
    (is (= ((ml/fn x :=> x) 1) 1) "normal case")
    (is (function? (ml/fn x :=> x)))))

(deftest if-test
  (testing "ml/if"
    (is (= (ml/if true then 1 else 2) 1) "if case")
    (is (= (ml/if false then 1 else 2) 2) "else case")))

(deftest val-test
  (testing "ml/val"
    (is (= (ml/val x :int := 1) 'x))
    (is (= x 1) "Defined on previous line")
    ))

(deftest fun-test
  (testing "ml/fun"
    (ml/fun foo x :int := x)
    (is (function? foo))
    (is (= (foo 1) 1))))

(deftest let-test
  (testing "ml/let"
    (is (= (ml/let [:val x :int := 1] :in x) 1))
    (is (= (ml/let [:val x :int := 1
                    :val y :int := 2]
             :in
             (+ x y))
           3))))


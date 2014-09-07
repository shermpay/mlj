(ns mlj.lang-test
  (:require [clojure.test :refer :all]
            [mlj.lang :as ml]))

(deftest fn-test
  (testing "ml/fn"
    (is (= ((ml/fn x => x) 1) 1) "normal case")
    (is (function? (ml/fn x => x)))))

(deftest if-test
  (testing "ml/if"
    (is (= (ml/if true then 1 else 2) 1) "if case")
    (is (= (ml/if false then 1 else 2) 2) "else case")))

(deftest val-test
  (testing "ml/val"
    (is (= (ml/val x :int = 1) 'x))
    (is (= x 1) "Defined on previous line")))

(deftest fun-test
  (testing "ml/fun"
    (ml/fun foo [:int :int] x = x)
    (is (function? foo))
    (is (= (foo 1) 1))))

(deftest let-test
  (testing "ml/let"
    (is (= (ml/let [val x = 1] in x end) 1))
    (is (= (ml/let [val x = 1
                    val y = 2]
             in
             (+ x y)
             end)
           3))))

(deftest mlj-keyword?-test
  (are [s p] (= (ml/mlj-keyword? s) p)
       'fn true
       'if true
       'fun true
       'val true
       'case true
       'let true

       'defn false
       :fun false
       1 false
       "x" false
       true false))

(deftest keyword-argcount-test
  (are [k n] (= (ml/keyword-argcount k) n)
       'fn 4
       'if 5
       'fun 5
       'val 4
       'case 4
       'let 4))

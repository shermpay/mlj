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

(deftest typesig-test
  (testing "typesig parsing"
    (are [sig t] (= (ml/typesig-type sig) t)
         [] :tuple
         '() :fn
         1 :prim)
    (is (= (ml/parse-fn-typesig '([:int * :int] -> :string -> :char -> :int))
           '([:int * :int] (:string (:char :int)))) "fn typesig parsing")
    (are [sig r] (= (ml/parse-typesig sig) r)
         :int :int
         '[:int * :char] [:int :char]
         '([:int * :char] -> [:char * :int]) '([:int :char] [:char :int]))))

(deftest val-test
  (testing "ml/val"
    (is (= (ml/val x :int = 1) 'x))
    (is (= x 1) "Defined on previous line")
    ))

;; (deftest fun-test
;;   (testing "ml/fun"
;;     (ml/fun foo [:int :int] x = x)
;;     (is (function? foo))
;;     (is (= (foo 1) 1))))

;; (deftest let-test
;;   (testing "ml/let"
;;     (is (= (ml/let [val x = 1] in x end) 1))
;;     (is (= (ml/let [val x = 1
;;                     val y = 2]
;;              in
;;              (+ x y)
;;              end)
;;            3))))


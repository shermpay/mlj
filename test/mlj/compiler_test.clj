(ns mlj.compiler-test
  (:require [clojure.test :refer :all]
            [mlj.compiler :as compiler]
            [mlj.lang :as ml]))

(deftest const-test
  (testing "Basic constants compilation"
    (is (= (compiler/compile [:int "1"]) 1) "int")
    (is (= (compiler/compile [:char "a"]) \a) "char")
    (is (= (compiler/compile [:bool "true"]) true) "bool")
    (is (= (compiler/compile [:string "hello"]) "hello") "string")))


(deftest tuple-test
  (testing "tuple expr compilation"
    (is (= (compiler/compile [:expr [:tuple [:expr [:id "x"]] [:expr [:id "y"]]]])
           '[x y]) "tuple"))) 

(deftest if-test
  (testing "if expr compilation"
    (is (= (compiler/compile
            [:if [:expr [:bool "true"]]
                              [:expr [:int "1"]]
                              [:expr [:int "2"]]])
           '(if true 1 2)) "basic")))

(deftest let-test
  (testing "if expr compilation"
    (is (= (compiler/compile
            [:let
             [:val [:pat [:id "x"]] [:expr [:int "1"]]]
             [:in
              [:expr [:id "x"]]]])
           '(let [x 1] x)) "basic")))

(deftest fn-test
  (testing "fn expr compilation"
    (is (= (compiler/compile
            [:fn [:pat [:id "x"]] [:expr [:id "x"]]])
           '(fn [x] x)) "basic")))

(deftest val-test
  (testing "val decl compilation"
    (is (= (compiler/compile
            [:val [:pat [:id "x"]] [:expr [:int "1"]]])
           '(def x 1)) "basic")))

(deftest fun-test
  (testing "fun decl compilation"
    (is (= (compiler/compile
            [:decl [:fun [:id "foo"] [:pat [:id "x"]] [:expr [:int "1"]]]])
           '(defn foo [x] 1)) "basic")
    (is (= (compiler/compile [:fun [:id "foo"] [:pat [:id "x"]] [:pat [:id "y"]]
                              [:expr [:id "x"]]])
           '(defn foo [x] (fn [y] x))))))

;; (deftest typesig-test
;;   (testing "typesig parsing"
    
;;     (is (= (comp/parse-fn-typesig '([:int * :int] -> :string -> :char -> :int))
;;            '([:int * :int] (:string (:char :int)))) "fn typesig parsing")
;;     (are [sig r] (= (comp/parse-typesig sig) r)
;;          :int :int
;;          '[:int * :char] [:int :char]
;;          '([:int * :char] -> [:char * :int]) '([:int :char] [:char :int]))))

;; (deftest mlj-compile-test
;;   (is (= (comp/parse if true then 1 else 2) '((#'mlj.lang/if true then 1 else 2)) ))
;;   (is (= (comp/parse if true then + else -
;;              if true then 2 else 3
;;              if true then 1 else 4)
;;          '((#'mlj.lang/if true then + else -)
;;            (#'mlj.lang/if true then 2 else 3)
;;            (#'mlj.lang/if true then 1 else 4))))
;;   (is (= (comp/parse val x :int = (if true then 1 else 2))
;;          '((#'mlj.lang/val x :int = (if true then 1 else 2))))))

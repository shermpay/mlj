(ns mlj.compiler-test
  (:require [clojure.test :refer :all]
            [mlj.compiler :as comp]
            [mlj.lang :as ml]))

(deftest typesig-test
  (testing "typesig parsing"
    
    (is (= (comp/parse-fn-typesig '([:int * :int] -> :string -> :char -> :int))
           '([:int * :int] (:string (:char :int)))) "fn typesig parsing")
    (are [sig r] (= (comp/parse-typesig sig) r)
         :int :int
         '[:int * :char] [:int :char]
         '([:int * :char] -> [:char * :int]) '([:int :char] [:char :int]))))

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

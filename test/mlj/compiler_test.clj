(ns mlj.compiler-test
  (:require [clojure.test :refer :all]
            [mlj.compiler :as comp]
            [mlj.lang :as ml]))

(deftest mlj-parser-test
  (is (= (comp/parse-res "comp.sml")
         '([:decl [:val [:id "x"] [:expr [:number "1"]]]]
             [:decl [:val [:id "foo"] [:expr [:fn [:id "x"] [:expr [:id "x"]]]]]]
               [:decl [:val [:id "y"] [:expr [:id "foo"] [:string "a"]]]]
                 [:decl [:val [:id "z"] [:expr [:id "foo"] [:char "#" "\"" "a" "\""]]]]
                   [:decl
                    [:val
                     [:id "a"]
                     [:expr
                      [:if
                       [:expr [:bool "true"]]
                       [:expr [:number "1"]]
                       [:expr [:number "2"]]]]]]
                     [:decl
                      [:val
                       [:id "b"]
                       [:expr
                        [:if
                         [:expr [:bool "true"]]
                         [:expr
                          [:if
                           [:expr [:bool "true"]]
                           [:expr [:number "1"]]
                           [:expr [:number "2"]]]]
                         [:expr [:number "3"]]]]]]
                       [:decl [:val [:id "t"] [:expr [:bool "true"]]]]
                         [:decl [:val [:id "f"] [:expr [:bool "false"]]]]
                           [:decl
                            [:val [:id "l"] [:expr [:let "in" [:expr [:number "1"]] "end"]]]]
                             [:decl [:val [:id "tt" [:ann [:type "bool"]]] [:expr [:bool "true"]]]]))))

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

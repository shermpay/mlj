(ns mlj.compiler-test
  (:require [clojure.test :refer :all]
            [mlj.compiler :as comp :refer [mlj-compile]]
            [mlj.lang :as ml]))

;; (deftest mlj-compile-test
;;   (is (= (mlj-compile if true then 1 else 2) ((#'mlj.lang/if true then 1 else 2)) ))
;;   (is (= (mlj-compile if true then + else -
;;              if true then 2 else 3
;;              if true then 1 else 4)
;;          '((#'mlj.lang/if true then + else -)
;;            (#'mlj.lang/if true then 2 else 3)
;;            (#'mlj.lang/if true then 1 else 4))))
;;   (is (= (mlj-compile val x :int = (if true then 1 else 2))
;;          '((#'mlj.lang/val x :int = (if true then 1 else 2))))))

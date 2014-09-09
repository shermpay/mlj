(ns mlj.core-test
  (:require [clojure.test :refer :all]
            [mlj.core :as core]))

(deftest mlj-keyword?-test
  (are [s p] (= (core/mlj-keyword? s) p)
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
  (are [k n] (= (core/count-args k) n)
       'fn 4
       'if 5
       'fun 5
       'val 4
       'case 4
       'let 4))

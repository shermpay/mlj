(ns mlj.lib
  "Standard Library"
  (:require [clojure.core :as c]
            [mlj.lang :as ml])
  (:gen-class))

(ml/fun + [x y] ([:int * :int] -> :int) = (c/+ x y))
(ml/fun - [x y] ([:int * :int] -> :int) = (c/- x y))
(ml/fun * [x y] ([:int * :int] -> :int) = (c/* x y))
(ml/fun / [x y] ([:int * :int] -> :int) = (c// x y))

(def builtins "Global var of map from builtin symbol -> var"(ns-publics *ns*))

(ns mlj.lib
  "Standard Library"
  (:refer-clojure :exclude [val let fn if])
  (:require [clojure.core :as c]
            [mlj.lang :as ml])
  (:gen-class))

(ml/fun + [x y] ([:int :int] :int) = (c/+ x y))
(ml/fun - [x y] ([:int :int] :int) = (c/- x y))
(ml/fun * [x y] ([:int :int] :int) = (c/* x y))
(ml/fun / [x y] ([:int :int] :int) = (c// x y))

(def builtins "Global var of map from builtin symbol -> var"(ns-publics *ns*))

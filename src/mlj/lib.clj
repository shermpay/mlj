;;;; Sherman Pay
;;;; TODO: Add more functions
;;;; TODO: Overload operators
(ns mlj.lib
  "Standard Library"
  (:refer-clojure :exclude [val let fn if])
  (:require [clojure.core :as c]
            [mlj.lang :as ml])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive Generic ;;
;;;;;;;;;;;;;;;;;;;;;;;
(ml/fun id x (:a :a) := x)
;;;;;;;;;;;;;;;;;;;;
;; Int operations ;;
;;;;;;;;;;;;;;;;;;;;
(ml/fun op+ [x y] ([:int :int] :int) := (c/+ x y))
(ml/fun op- [x y] ([:int :int] :int) := (c/- x y))
(ml/fun op* [x y] ([:int :int] :int) := (c/* x y))
(ml/fun div [x y] ([:int :int] :int) := (c// x y))
(ml/fun neg x (:int :int) := (c/- x)) 

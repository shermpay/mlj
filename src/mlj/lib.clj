;;;; Sherman Pay
;;;; TODO: Add more functions
;;;; TODO: Overload operators
(ns mlj.lib
  "Standard Library"
  (:refer-clojure :exclude [val let fn if])
  (:require [clojure.core :as c]
            [mlj.lang :as ml])
  (:gen-class))

(defn- add-type [v t]
  (alter-meta! v #(assoc % :type t)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive Generic ;;
;;;;;;;;;;;;;;;;;;;;;;;
(ml/fun id x (:a :a) := x)
;;;;;;;;;;;;;;;;;;;;
;; Int operations ;;
;;;;;;;;;;;;;;;;;;;;
(ml/fun op+ [x y] [:fn [:int :int] :int] := (c/+ x y))
(ml/fun op- [x y] [:fn [:int :int] :int] := (c/- x y))
(ml/fun op* [x y] [:fn [:int :int] :int] := (c/* x y))
(ml/fun div [x y] [:fn [:int :int] :int] := (c// x y))
(ml/fun neg x [:fn :int :int] := (c/- x)) 

;;;;;;;;;;;;;;
;; Basic IO ;;
;;;;;;;;;;;;;;

(defn types []
  "Returns a map of stdlib types. Where the map is from {str -> vector}"
  (clojure.core/let [var-map (ns-publics (the-ns 'mlj.lib))]
    (into {} (map (clojure.core/fn [[sym v]]
                    [(name sym) (:type (meta v))]) var-map))))

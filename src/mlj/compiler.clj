(ns mlj.compiler
  "Compiles all of MLJ forms into Clojure forms including definitions
  provided in mlj.lang."
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io]
            [mlj.ast :as ast]
            [mlj.parser :as parser])
  (:gen-class))

(defn vectorize [v]
  (if (vector? v)
    v
    (vector v)))

(defmulti compile
  "Takes a vector tree and compiles into a Clojure form.
  Compilation is dispatched on the nodes tag."
  ast/tag-of)

(defmethod compile :id [[_ item]]
  (symbol item))

;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;
(defmethod compile :int [[_ item]]
  (Integer/parseInt item))

(defmethod compile :string [[_ item]]
  item)

(defmethod compile :char [[_ item]]
  {:pre (= 1 (count item))}
  (.charAt item 0))

(defmethod compile :bool [[_ item]]
  (Boolean/parseBoolean item))

(defmethod compile :unit [_]
  [])

;;;;;;;;;;;;;;;;;
;; Expressions ;;
;;;;;;;;;;;;;;;;;
(defmethod compile :tuple [[_ & items]]
  (vec (map compile items)))

(defmethod compile :if [[_ & [pred then else]]]
  (list 'if
        (compile pred)
        (compile then)
        (compile else)))

(defmethod compile :let [[_ & exps]]
  (let [[bindings [[_ body]]] (partition-by #(= (ast/tag-of %) :in) exps)]
    (list
     'let (->> bindings
               ;; Transform into a vector of bindings
               (map (fn [[_ id expr]]
                      (vector (compile id) (compile expr))))
               flatten
               vec)
     (compile body))))

(defmethod compile :fn [[_ params body]]
  (list 'fn
        (vectorize (compile params))
        (compile body)))

(defmethod compile :expr [[_ & items]]
  (if (= 1 (count items))
    (compile (first items))
    (map compile items)))

;;;;;;;;;;;;;;;;;;
;; Declarations ;;
;;;;;;;;;;;;;;;;;;

(defmethod compile :val [[_ & [pat exp]]]
  (let [p (compile pat)
        e (compile exp)]
    (if (vector? p)
      ;; Compiles into a vector of defs => [(def x 1), (def y 2)]
      (mapv #(list 'def %1 %2) (flatten p) (flatten e))
      (list 'def p e))))

(defmethod compile :fun [[_ & [name & pat-body]]]
  (let [pats  (butlast pat-body)
        body (last pat-body)]
    (if (= (count pats) 1)
      (list 'defn
            (compile name)
            (vectorize (compile (first pats)))
            (compile body))
      (list 'defn
            (compile name)
            (vectorize (compile (first pats)))
            (reduce #(list 'fn (vectorize (compile %2)) %1) (compile body)
                    (reverse (rest pats)))))))

(defmethod compile :decl [[_ item]]
  (compile item))

;;;;;;;;;;;;;
;; Pattern ;;
;;;;;;;;;;;;;
(defmethod compile :pat [[_ item]]
  (compile item))

(defmethod compile :pattuple [[_ & items]]
  (mapv compile items))

(defmethod compile :blank [[_ & item]]
  '_)

;;;;;;;;;;
;; Misc ;;
;;;;;;;;;;
(defmethod compile :default [tree]
  (throw (ex-info "Unable to compile, malformed syntax" (into {} tree))))

(defn compilef [tree]
  (compile (first tree)))

(defn compile-prog
  "Takes in a list of vectors representing a MLJ AST and compiles it into Clojure"
  [ast]
  {:pre [(seq? ast)]}
  (map compile ast))

;; (defn testing []
;;   (->> "comp.sml"
;;       parse-res
;;       (take 5)
     
;;       pretty/pprint))


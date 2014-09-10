(ns mlj.type
  "ML (Hindley Milner) type system.
  Includes a static type checker that type checks valid mlj forms.
  Also provides builtin mlj types."
  (:require [mlj.lang :as ml]
            [mlj.core :as core]
            [mlj.lib :as lib])
  (:gen-class))

(declare type-of check-expr)

;;;;;;;;;;;;;;;;;
;; Definitions ;;
;;;;;;;;;;;;;;;;;
(def ^:dynamic *type-map* {:int {:type java.lang.Long
                                :type-fn integer?}
                           :real {:type java.lang.Double
                                 :type-fn float?}
                           :bool {:type java.lang.Boolean
                                 :type-fn #{true false}}
                           :string {:type java.lang.String
                                   :type-fn string?}
                           :char {:type java.lang.Character
                                 :type-fn char?}})
;;;;;;;;;;;;;;
;; Generics ;;
;;;;;;;;;;;;;;
(defn generic?
  [t]
  {:pre [(keyword? t)]}
  (not (*type-map* t)))

;;;;;;;;;;;;;;;;;
;; Tuple Types ;;
;;;;;;;;;;;;;;;;;
(defn tuple? [v] (vector? v))
(defn unit? [v] (and (tuple? v) (empty? v)))

(defn tuple-type
  [tuple]
  {:pre [(tuple? tuple)]}
  (vec (map type-of tuple)))

(defn tuple-repr
  [tuple]
  "Converts a tuple-type obj to it's external representation. [:int :int] to [:int * :int]"
  (interpose '* tuple))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking primitives ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn type? 
  "Returns true if t is a type form."
  [t]
  (contains? *type-map* t))

(defn var-type
  "Gets the type of a var"
  [vr]
  (-> vr
      core/get-var
      meta
      :type))

(defn type-of 
  "Gets the type of a value"
  [v]
  {:pre [(not (or (symbol? v) (keyword? v)))]}
  (cond
   (tuple? v) (tuple-type v)
   (var? v) (var-type v)
   :else (-> (filter #((:type-fn (% 1)) v) *type-map*)
             first
             key)))

(defn check-type 
  "Check if v is of type t"
  [v t]
  (= (type-of v) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Type signatures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fn-type?
  "Checks if is a function type"
  [vr]
  (= :fn (core/typesig-type (var-type vr))))

(defn fn-type
  "Gets the type-signature of a function"
  [f]
  {:pre [(core/builtin? f)]}
  (core/get-type f))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking forms ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defn check-app
  "Type checks a function application. Returns the return type of the function if valid."
  [[f arg]]
  (let [arg-types (vec (map check-expr arg))
        [par ret] (fn-type f)]
    (if (not= arg-types par)
      (throw (IllegalArgumentException.
              (str "Type Error: " f " args: (" arg "), expecting: " par "\n Got: " arg-types)))
      ret)))

(defn check-val
  "Type checks a VAL declaration"
  [[val-sym sym t eq-sym v :as val-expr]]
  (if (= (check-expr v) t)
    t
    (throw (IllegalArgumentException.
            (str "Type Error: VAL binding type mismatch."
                 "\n\tgot: " (type-of v)
                 "\n\tin: " (apply str (interpose \space val-expr)))))))

(defn check-if
  "Type checks an IF expression. Returns the type of the branches if valid, else throws an exception"
  [[if-sym pred then-sym t-expr else-sym e-expr :as if-expr]]
  (cond (not (check-type pred :bool))
        (throw (IllegalArgumentException.
                (str "Type Error: test in IF expression not type bool"
                     "\n\ttest expression: " (type-of pred)
                     "\n\tin expression: " if-expr)))
        (not (check-type e-expr (type-of t-expr)))
        (throw (IllegalArgumentException. "Type Error: IF branches types do not agree"))
        :else (do
                (check-expr t-expr)
                (check-expr e-expr))))

(defn check-let
  "Type check a LET expression. Returns the type of the body if valid"
  [[let-sym binding-exprs in-sym body end-sym :as let-expr]]
  (doseq [binding (partition (inc (core/count-args 'val)) binding-exprs)]
    (check-val binding))
  (check-expr body))

(defn check-expr
  "Type checks an expression. 
  Throws an exception if type is invalid, else returns the type of the expression"
  [expr]
  (if (list? expr)
    (let [[form & body] expr]
        (cond (core/mlj-keyword? form) (case form
                                        val (check-val expr)
                                        if (check-if expr)
                                        let (check-let expr)
                                        (throw (IllegalStateException. "Found uncheck keyword")))
             (core/builtin? form) (check-app expr)
             :else ((throw (IllegalStateException. "Found uncheck form")))))
    (type-of expr)))


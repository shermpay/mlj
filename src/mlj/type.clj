(ns mlj.type
  "ML (Hindley Milner) type system."
  (:require [mlj.lang :as ml])
  (:gen-class))

(def ^:dynamic *type-map* {:int {:type java.lang.Long
                                :fn integer?}
                           :real {:type java.lang.Double
                                 :fn float?}
                           :bool {:type java.lang.Boolean
                                 :fn #{true false}}
                           :string {:type java.lang.String
                                   :fn string?}
                           :char {:type java.lang.Character
                                 :fn char?}})
(defn type? 
  "Returns true if t is a type form."
  [t]
  (contains? *type-map* t))

(defn type-of 
  "Gets the type of a expression"
  [x]
  (-> (filter #((:fn (% 1)) x) *type-map*)
      first
      key))

(defn check-type 
  "Check if v is of type t"
  [v t]
  ((-> *type-map* t :fn) v))

(defmacro check-expr
  "Type checks an expression. 
  Throws an exception if type is invalid, else returns true"
  [expr]
  true)

(defn check-val
  [[val-sym sym t eq-sym v :as val-expr]]
  (if (check-type v t)
    true
    (throw (IllegalArgumentException.
            (str "Type Error: VAL binding type mismatch."
                 "\n\tgot: " (type-of v)
                 "\n\tin: " (apply str (interpose \space val-expr)))))))

(defn check-if
  "Type checks an IF expression. Returns true if valid, else throws an exception"
  [[if-sym pred then-sym t-expr else-sym e-expr :as if-expr]]
  (cond (not (check-type pred :bool))
        (throw (IllegalArgumentException.
                (str "Type Error: test in IF expression not type bool"
                     "\n\ttest expression: " (type-of pred)
                     "\n\tin expression: " if-expr)))
        (not (check-type e-expr (type-of t-expr)))
        (throw (IllegalArgumentException. "Type Error: IF branches types do not agree"))
        :else true))

(defn check-let
  "Type check a LET expression. Returns true if valid"
  [[let-sym binding-exprs in-sym end-sym :as let-expr]]
  (doseq [binding (partition (inc (ml/keyword-argcount 'val)) binding-exprs)]
    (check-val binding))
  true)

(defn check-app
  "Type checks a function application."
  [f args])

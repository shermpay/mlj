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
  [tuple env]
  {:pre [(tuple? tuple)]}
  (vec (map #(type-of % env) tuple)))

(defn tuple-repr
  [tuple]
  "Converts a tuple-type obj to it's external representation. [:int :int] to [:int * :int]"
  (vec (interpose '* tuple)))

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
  [v env]
  {:pre [(not (keyword? v))]}
  (cond
   (tuple? v) (tuple-type v)
   (var? v) (var-type v)
   (symbol? v) (env v)
   :else (-> (filter #((:type-fn (% 1)) v) *type-map*)
             first
             key)))

(defn check-type 
  "Check if v is of type t in env"
  [v t env]
  (= (type-of v env) t))

(defn same-type?
  "Checks if v1 and v2 are the same type in env.
  Expect heavy usage for Generics."
  [v1 v2 env]
  (= (type-of v1 env) (type-of v2 env)))

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

;;;;;;;;;;;;;;;;;;;;;;;
;; Type Environments ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defn make-default-env
  "Create a default static environment {symbol type-sig}"
  []
  (reduce (fn [res sym]
            (assoc res sym (core/get-type sym)))
          {}
          (keys (core/op-map :builtin))))

;;; atom should NOT be modified for lexical reasons, only modifications are top-level definitions
(def environment "Environment of {symbol type-sig}" (atom (make-default-env)))

(defn fn-env
  "Creates a lexical environment for a function"
  [params param-types]
  (if (symbol? params)
    (hash-map params param-types)
    (zipmap params param-types)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking forms ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defn check-app
  "Type checks a function application. Returns the return type of the function if valid."
  [[f arg] env]
  (let [arg-types (vec (map #(check-expr % env) arg))
        [par ret] (fn-type f)]
    (if (not= arg-types par)
      (throw (IllegalArgumentException.
              (str "Type Error: " f " args: (" arg "), expecting: " par "\n Got: " arg-types)))
      ret)))

(defn check-val
  "Type checks a VAL declaration"
  [[val-sym sym t eq-sym v :as val-expr] env]
  (if (= (check-expr v @env) t)
    (do
      (swap! env assoc sym t)
      t)
    (throw (IllegalArgumentException.
            (str "Type Error: VAL binding type mismatch."
                 "\n\tgot: " (type-of v @env)
                 "\n\tin: " (apply str (interpose \space val-expr)))))))

(defn check-if
  "Type checks an IF expression. Returns the type of the branches if valid, else throws an exception"
  [[if-sym pred then-sym t-expr else-sym e-expr :as if-expr] env]
  (cond (not (check-type pred :bool env))
        (throw (IllegalArgumentException.
                (str "Type Error: test in IF expression not type bool"
                     "\n\ttest expression: " (type-of pred env)
                     "\n\tin expression: " if-expr)))
        (not (same-type? e-expr t-expr env))
        (throw (IllegalArgumentException. "Type Error: IF branches types do not agree"))
        :else (do
                (check-expr t-expr env)
                (check-expr e-expr env))))

(defn check-let
  "Type check a LET expression. Returns the type of the body if valid"
  [[let-sym binding-exprs in-sym body end-sym :as let-expr] env]
  (let [let-env (atom env)]
      (doseq [binding (partition (inc (core/count-args 'val)) binding-exprs)]
        (check-val binding let-env))
    (check-expr body @let-env)))

(defn check-fn
  "Type checks a FN expression. Return the fn type if valid"
  [[fn-sym param [par-type ret-type :as tsig] => body] env]
  (println tsig)
  (let [env (merge env (fn-env param par-type))
        body-type (check-expr body env)]
    (if (= body-type ret-type) 
      tsig
      (throw (IllegalArgumentException.
              (str "Type Error: FN return type mismatch."
                   "\n\tExpected: " ret-type
                   "\n\tGot: " body-type))))))

(defn check-expr
  "Type checks an expression. 
  Throws an exception if type is invalid, else returns the type of the expression"
  [expr env]
  (if (list? expr)
    (let [[form & body] expr]
        (cond (core/mlj-keyword? form) (case form
                                         val (check-val expr environment)
                                         if (check-if expr env)
                                         let (check-let expr env)
                                        (throw (IllegalStateException. "Found uncheck keyword")))
             (core/builtin? form) (check-app expr env)
             :else ((throw (IllegalStateException. "Found uncheck form")))))
    (type-of expr env)))


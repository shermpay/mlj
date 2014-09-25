;;;; Sherman Pay

;;;; TODO: Change implementation of Types
;;;; TODO: Add Generics
;;;; TODO: Catch errors in helper functions
(ns mlj.type
  "ML (Hindley Milner) type system.
  Includes a static type checker that type checks valid mlj forms.
  Also provides builtin mlj types."
  (:require [mlj.lang :as ml]
            [mlj.core :as core]
            [mlj.parser :as parser]
            [mlj.ast :as ast]
            [mlj.lib :as lib])
  (:gen-class))

(declare type-of check-expr tuple?)
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
                                  :type-fn char?}
                           :unit {:type nil
                                  :type-fn (comp tuple? empty?)}})
;;;;;;;;;;;;;;;;;;;;
;; Type Variables ;;
;;;;;;;;;;;;;;;;;;;;
(def type-vars (-> (map (comp keyword str char) (range (int \a) (int \z)))
                   vec
                   atom))
(defn type-var?
  [t]
  {:pre [(keyword? t)]}
  (and (not (*type-map* t))))

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
  (if (tuple? t)
    (not-any? false? (map type? t))
    (contains? *type-map* t)))

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
  {:pre [(not (keyword? v)) (not (list? v))]}
  (cond
   (nil? v) :unit
   (tuple? v) (tuple-type v env)
   (var? v) (var-type v)
   (symbol? v) (env v)
   :else (-> (filter #((:type-fn (% 1)) v) *type-map*)
             first
             key)))

(defn check-type 
  "Check if v is of type t in env"
  [v t env]
  (= (type-of v env) t))

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
;;; Application should hold a type variable map, that maps a type variable to a type
(defn check-app
  "Type checks a function application. Returns the return type of the function if valid."
  [[f arg] env]
  (let [[par ret] (fn-type f)
        arg-types (if (tuple? arg)
                    (vec (map-indexed (fn [idx item] (if-let [t (check-expr item env)]
                                                       t
                                                       (let [sym (arg idx)
                                                             infered (par idx)]
                                                         (swap! env assoc sym infered)
                                                         infered)))
                                      arg))
                    (check-expr arg @env))]
    (if (not= arg-types par)
      (throw (IllegalArgumentException.
              (str "Type Error: " f " args: (" arg "), expecting: " par "\n Got: " arg-types)))
      ret)))

(defn check-if
  "Type checks an IF expression. Returns the type of the branches if valid, else throws an exception"
  [[if-sym pred then-sym t-expr else-sym e-expr :as if-expr] env]
  (do
    (if-let [pred-type (check-expr pred env)]
        (if (not= pred-type :bool)
          (throw (IllegalArgumentException.
                  (str "Type Error: test in IF expression not type bool"
                       "\n\ttest expression: " (type-of pred @env)
                       "\n\tin expression: " if-expr))))
        (swap! env assoc pred :bool))
    (let [t-type (check-expr t-expr env)
          e-type (check-expr e-expr env)
          t-type (if (and (nil? t-type) (not (nil? e-type)))
                   e-type t-type)
          e-type (if (and (nil? e-type) (not (nil? t-type)))
                   t-type e-type)]
      (if (symbol? t-expr) (swap! env assoc t-expr t-type))
      (if (symbol? e-expr) (swap! env assoc e-expr e-type))
      (if (= t-type e-type)
        t-type
        (throw (IllegalArgumentException.
                (str "Type Error: IF branches types do not agree." t-type " <> " e-type)))))))
(defn- set-type! [sym t env]
  (do (swap! env assoc sym t)
      t))

(defn check-val
  "Type checks a VAL declaration"
  ([[val-sym sym & [& {tsig :- v :=}] :as val-expr] env]
     (let [t (check-expr v env)]
       (if (or (nil? tsig) (= tsig t))
         (do
           (swap! env assoc sym t)
           t)
         (throw (IllegalArgumentException.
                 (str "Type Error: VAL binding type mismatch."
                      "\n\tgot: " (type-of v @env)
                      "\n\tin: " (apply str (interpose \space val-expr)))))))))

(defn check-fn
  "Type checks a FN expression. Return the fn type if valid"
  [[fn-sym param & {[par-type ret-type :as tsig] :-, body :=>}] env]
  (let [env (atom (merge env (fn-env param par-type)))
        body-type (check-expr body env)
        par-type (if (nil? par-type) (check-expr param env) par-type)]
    (if (or (nil? ret-type) (= body-type ret-type)) 
      (list par-type body-type)
      (throw (IllegalArgumentException.
              (str "Type Error: FN return type mismatch."
                   "\n\tExpected: " ret-type
                   "\n\tGot: " body-type))))))

(defn check-fun
  "Type checks a FUN declaration. Return the fn type if valid"
  [[fun-sym name param & {[par-type ret-type :as tsig] :-, body :=}] env]
  (check-val ['val name :- tsig := (list 'fn param :- tsig :=> body)] env))

(defn check-let
  "Type check a LET expression. Returns the type of the body if valid"
  [[let-sym binding-exprs & {body :in} :as let-expr] env]
  (let [let-env (atom env)]
    (doseq [binding (keep-indexed #(if (odd? %1) (conj %2 'val))
                                  (partition-by #(= :val %) binding-exprs))]
      (check-val binding let-env))
    (check-expr body let-env)))

(defn check-expr
  "Type checks an expression. 
  Throws an exception if type is invalid, else returns the type of the expression"
  [expr env]
  (if (list? expr)
    (let [[form & body] expr]
      (cond
       (core/mlj-keyword? form) (case form
                                  val (check-val expr environment)
                                  if (check-if expr env)
                                  let (check-let expr @env)
                                  fn (check-fn expr @env)
                                  fun (check-fun expr environment)
                                  (throw (IllegalStateException. (str "Found uncheck keyword: " form))))
       (core/builtin? form) (check-app expr env)
       :else ((throw (IllegalStateException. (str "Found uncheck form: " form))))))
    (type-of expr @env)))


(defmulti parse-ann "Takes a vector tree and parses the type ann" ast/tag-of)

(defmethod parse-ann :type [[_ s]]
  (keyword s))

(defmethod parse-ann :ttuple [[_ & ts]]
  (vec (map parse-ann ts)))

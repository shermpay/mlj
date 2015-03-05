;;;; Sherman Pay

<<<<<<< Updated upstream
;;;; TODO: Change implementation of Types
;;;; TODO: Add Generics
=======
>>>>>>> Stashed changes
;;;; DONE: Change implementation of Types
;;;; TODO: Implement Union Find data structure
;;;; TODO: Add Type variables
;;;; TODO: Catch errors in helper functions

(ns mlj.type
  "ML (Hindley Milner) type system.
  Includes a static type checker that type checks valid mlj forms.
  Also provides builtin mlj types."
  (:refer-clojure :exclude [find])
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

(defn all-defined []
  (keys *type-map*))

(defn defined? [t]
  (nil? (clojure.core/find *type-map* t)))

(defn definition [t]
  (clojure.core/get *type-map* t))

(defrecord FnType [param ret]
  java.lang.Object
  (toString [this] (str param " -> " ret)))

(defn fn-type [param ret]
  (FnType. param ret))

(defn fn-type? [t]
  (instance? mlj.type.FnType t))

(defonce type-var-names (map char (range 97 123)))

(defn tvar-name-generator
  "Returns a generator function that produces a fresh tvar name when called."
  []
  (let [names (atom type-var-names)]
    (fn [] (let [res (-> @names
                         first
                         (str \')
                         keyword)]
             (swap! names rest)
             res))))

(defrecord TypeVar [id])

(defn type-var [id]
  {:pre [(keyword? id) (not (defined? id))]}
  (TypeVar. id))

(defn new-type-var [env]
  (let [existing (into #{} (vals env))
        generator (tvar-name-generator)]
    (loop [x (generator)]
      (if (existing x)
        (recur (generator))
        x))))
        
;;;;;;;;;;;;;;;;
;; Union Find ;;
;;;;;;;;;;;;;;;;
(defprotocol UnionFind
  "A protocol for collection types that support union/find operations."

  (union [this x y] "Returns a new union find structure where x and y have the same root.")
  (find [this x] "Returns the root of x"))

(defrecord DisjointSetNode [root height]
  java.lang.Object
  (toString [this] (str (into {} this))))

(defn- disjoint-set-node? [x]
  (instance? DisjointSetNode x))

(deftype DisjointSetForest [sets]
  UnionFind

  (find [this x]
    (let [root (.root (nth sets x))]
      root))

  (union [this x y]
    {:pre [(< x (count sets)) (< y (count sets))]}
    (let [x-node (get sets x)
          x-root (if-let [r (.root x-node)]
                   r
                   x)
          y-node (get sets y)]
        (assoc sets
          y
          (->DisjointSetNode (.value y-node) x-root (+ (.height x-node)
                                                       (.height y-node))))))

  java.lang.Object

  (toString [this] (apply str (map str sets))))

(defn disjoint-sets [& items]
  (DisjointSetForest.
   (into {}
         (map vector
              items
              (map ->DisjointSetNode
                   ;; Root
                   (repeat (count items) nil)
                   ;; Height
                   (repeat (count items) 1))))))

(defn all-defined []
  (keys *type-map*))

(defn defined? [t]
  (nil? (clojure.core/find *type-map* t)))

(defn definition [t]
  (clojure.core/get *type-map* t))

(defrecord FnType [param ret]
  java.lang.Object
  (toString [this] (str param " -> " ret)))

(defn fn-type [param ret]
  (FnType. param ret))

(defn fn-type? [t]
  (instance? mlj.type.FnType t))

(defonce type-var-names (map char (range 97 123)))

(defn tvar-name-generator
  "Returns a generator function that produces a fresh tvar name when called."
  []
  (let [names (atom type-var-names)]
    (fn [] (let [res (-> @names
                         first
                         (str \')
                         keyword)]
             (swap! names rest)
             res))))

(defrecord TypeVar [id])

(defn type-var [id]
  {:pre [(keyword? id) (not (defined? id))]}
  (TypeVar. id))

(defn new-type-var [env]
  (let [existing (into #{} (vals env))
        generator (tvar-name-generator)]
    (loop [x (generator)]
      (if (existing x)
        (recur (generator))
        x))))
        
;;;;;;;;;;;;;;;;
;; Union Find ;;
;;;;;;;;;;;;;;;;
(defprotocol UnionFind
  "A protocol for collection types that support union/find operations."

  (union [this x y] "Returns a new union find structure where x and y have the same root.")
  (find [this x] "Returns the root of x"))

(defrecord DisjointSetNode [root height]
  java.lang.Object
  (toString [this] (str (into {} this))))

(defn- disjoint-set-node? [x]
  (instance? DisjointSetNode x))

(deftype DisjointSetForest [sets]
  UnionFind

  (find [this x]
    (let [root (.root (nth sets x))]
      root))

  (union [this x y]
    {:pre [(< x (count sets)) (< y (count sets))]}
    (let [x-node (get sets x)
          x-root (if-let [r (.root x-node)]
                   r
                   x)
          y-node (get sets y)]
        (assoc sets
          y
          (->DisjointSetNode (.value y-node) x-root (+ (.height x-node)
                                                       (.height y-node))))))

  java.lang.Object

  (toString [this] (apply str (map str sets))))

(defn disjoint-sets [& items]
  (DisjointSetForest.
   (into {}
         (map vector
              items
              (map ->DisjointSetNode
                   ;; Root
                   (repeat (count items) nil)
                   ;; Height
                   (repeat (count items) 1))))))

(defn all-defined []
  (keys *type-map*))

(defn defined? [t]
  (nil? (clojure.core/find *type-map* t)))

(defn definition [t]
  (clojure.core/get *type-map* t))

(defrecord FnType [param ret]
  java.lang.Object
  (toString [this] (str param " -> " ret)))

(defn fn-type [param ret]
  (FnType. param ret))

(defn fn-type? [t]
  (instance? mlj.type.FnType t))

(defonce type-var-names (map char (range 97 123)))

(defn tvar-name-generator
  "Returns a generator function that produces a fresh tvar name when called."
  []
  (let [names (atom type-var-names)]
    (fn [] (let [res (-> @names
                         first
                         (str \')
                         keyword)]
             (swap! names rest)
             res))))

(defrecord TypeVar [id])

(defn type-var [id]
  {:pre [(keyword? id) (not (defined? id))]}
  (TypeVar. id))

(defn new-type-var [env]
  (let [existing (into #{} (vals env))
        generator (tvar-name-generator)]
    (loop [x (generator)]
      (if (existing x)
        (recur (generator))
        x))))
        
;;;;;;;;;;;;;;;;
;; Union Find ;;
;;;;;;;;;;;;;;;;
(defprotocol UnionFind
  "A protocol for collection types that support union/find operations."

  (union [this x y] "Returns a new union find structure where x and y have the same root.")
  (find [this x] "Returns the root of x"))

(defrecord DisjointSetNode [root height]
  java.lang.Object
  (toString [this] (str (into {} this))))

(defn- disjoint-set-node? [x]
  (instance? DisjointSetNode x))

(deftype DisjointSetForest [sets]
  UnionFind

  (find [this x]
    (let [root (.root (nth sets x))]
      root))

  (union [this x y]
    {:pre [(< x (count sets)) (< y (count sets))]}
    (let [x-node (get sets x)
          x-root (if-let [r (.root x-node)]
                   r
                   x)
          y-node (get sets y)]
        (assoc sets
          y
          (->DisjointSetNode (.value y-node) x-root (+ (.height x-node)
                                                       (.height y-node))))))

  java.lang.Object

  (toString [this] (apply str (map str sets))))

(defn disjoint-sets [& items]
  (DisjointSetForest.
   (into {}
         (map vector
              items
              (map ->DisjointSetNode
                   ;; Root
                   (repeat (count items) nil)
                   ;; Height
                   (repeat (count items) 1))))))

(defn all-defined []
  (keys *type-map*))

(defn defined? [t]
  (nil? (clojure.core/find *type-map* t)))

(defn definition [t]
  (clojure.core/get *type-map* t))

(defrecord FnType [param ret]
  java.lang.Object
  (toString [this] (str param " -> " ret)))

(defn fn-type [param ret]
  (FnType. param ret))

(defn fn-type? [t]
  (instance? mlj.type.FnType t))

(defonce type-var-names (map char (range 97 123)))

(defn tvar-name-generator
  "Returns a generator function that produces a fresh tvar name when called."
  []
  (let [names (atom type-var-names)]
    (fn [] (let [res (-> @names
                         first
                         (str \')
                         keyword)]
             (swap! names rest)
             res))))

(defrecord TypeVar [id])

(defn type-var [id]
  {:pre [(keyword? id) (not (defined? id))]}
  (TypeVar. id))

(defn new-type-var [env]
  (let [existing (into #{} (vals env))
        generator (tvar-name-generator)]
    (loop [x (generator)]
      (if (existing x)
        (recur (generator))
        x))))
        
;;;;;;;;;;;;;;;;
;; Union Find ;;
;;;;;;;;;;;;;;;;
(defprotocol UnionFind
  "A protocol for collection types that support union/find operations."

  (union [this x y] "Returns a new union find structure where x and y have the same root.")
  (find [this x] "Returns the root of x"))

(defrecord DisjointSetNode [root height]
  java.lang.Object
  (toString [this] (str (into {} this))))

(defn- disjoint-set-node? [x]
  (instance? DisjointSetNode x))

(deftype DisjointSetForest [sets]
  UnionFind

  (find [this x]
    (let [root (.root (nth sets x))]
      root))

  (union [this x y]
    {:pre [(< x (count sets)) (< y (count sets))]}
    (let [x-node (get sets x)
          x-root (if-let [r (.root x-node)]
                   r
                   x)
          y-node (get sets y)]
        (assoc sets
          y
          (->DisjointSetNode (.value y-node) x-root (+ (.height x-node)
                                                       (.height y-node))))))

  java.lang.Object

  (toString [this] (apply str (map str sets))))

(defn disjoint-sets [& items]
  (DisjointSetForest.
   (into {}
         (map vector
              items
              (map ->DisjointSetNode
                   ;; Root
                   (repeat (count items) nil)
                   ;; Height
                   (repeat (count items) 1))))))

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

;;;;;;;;;;;;;;;;;;;
;; Type checking ;;
;;;;;;;;;;;;;;;;;;;
(defmulti type-of "Takes a ast node and returns its type"
  (fn [node & _] (ast/tag-of node)))

(defmethod type-of :expr [[_ expr & more] env]
  (let [t (type-of expr env)]
    (if (and (fn-type? t)
             (not= nil more))
      (.ret t)                          ; Function application
      t)))

(defmethod type-of :int [_ env]
  :int)

(defmethod type-of :char [_ env]
  :char)

(defmethod type-of :string [_ env]
  :string)

(defmethod type-of :bool [_ env]
  :bool)

(defmethod type-of :tuple [[_ & tuple] env]
  (mapv #(type-of % env) tuple))

(defmethod type-of :id [[_ sym] env]
  (if-let [res (get env sym)]
    res
    ;; Create new type var
    (new-type-var env)))

(defmethod type-of :if [[_ test then else :as if-expr] env]
  (let [test-type (type-of test env)
        then-type (type-of then env)
        else-type (type-of else env)]
    (if (not= test-type :bool)
      (throw (ex-info "test expr in if is not type bool" {:test test-type
                                                          :expr if-expr}))
      (if (not= then-type else-type)
        (throw (ex-info "then and else expr in if type mismatch" {:then then-type
                                                                  :else else-type
                                                                  :expr if-expr}))
        then-type))))

(defmethod type-of :let [[_ & body] env]
  (let [pat (butlast body)
        [_ expr] (last body)            ; _ is the :in keyword
        new-env (reduce #(conj %1 (decl-val %2 env)) env pat)]
    (type-of expr new-env)))

(defmethod type-of :fn [[_ & func] env]
  (let [param (first func)
        body (last func)
        param-types (pat->vec param env)
        fn-env (merge env (apply hash-map (flatten param-types)))
        ret-type (type-of body fn-env)]
    (FnType. (patvec->tvec param-types) ret-type)))

(defmethod type-of :default [_ env]
  (throw (ex-info "Unable to obtain types." {:given _})))

(defn create-env [pat env]
  (reduce #(conj %1 (decl-val %2 env)) env pat))

(defmulti parse-type "Takes a vector tree and parses the type" ast/tag-of)

(defmethod parse-type :type [[_ s]]
  (keyword s))

(defmethod parse-type :ttuple [[_ & ts]]
  (vec (map parse-type ts)))

(defn parse-ann [[_ ann]]
  (parse-type ann))

(defn match-ann 
  "Takes a an object o and an annotation ann and check if o is correctly annotated"
  [o ann]
  (let [[tag & vals] o]
    (if (= tag :tuple)
      (mapv (fn [[_ x] & a] (match-ann x a)) vals)
      (if (or (nil? ann) (= tag (parse-ann ann)))
        (ast/tag-of o)
        (throw (ex-info "Type Annotation mismatch." {:got o
                                                     :expect ann}))))))

(defn check-ann [[node ann]]
  (= (type-of node) ann))

;;;;;;;;;;;;;;;;;;
;; Declarations ;;
;;;;;;;;;;;;;;;;;;
(defn pat->vec
  "Takes a pattern node and converts it into a vector of pairs(in a list).
  The first index is the ids, and the second is the annotations."
  [[pat-type & pat] env]
  (if (= pat-type :id)
    (list (first pat)
          (if (> (count pat) 1)
            ;; Has a type annotation
            (parse-ann (second pat))
            ;; No type annotation, requires inference
            (new-type-var env)))
    (mapv #(pat->vec % env) pat)))

(defn get-patvec
  [[_ [form-type pat exp]]]
  (pat->vec pat))

(defn patvec->tvec
  [patvec]
  (if (string? (first patvec))
    (second patvec)
    (mapv patvec->tvec patvec)))

(defn check-patvec
  "Check pattern and type vector, create a type environment if no
  errors were found.  Else throws an ex-info"
  [patvec tvec env decl]
  (if (vector? patvec)
    ;; Tuple pattern
    (do
      (doseq [[pair t] (map vector patvec tvec)]
        (swap! env conj (check-patvec pair t env decl)))
      @env)
    ;; single var
    (let [[id pat-type] patvec]
      (println patvec)
      (println pat-type)
      (println tvec)
      (if (and (not= pat-type tvec) (not= nil pat-type))
        (throw (ex-info "Pattern and expression in val dec don't agree."
                        {:pattern pat-type
                         :expression tvec
                         :in decl}))
        (apply hash-map [id tvec])))))

(defn- strlist->str
  "Takes a list of strings and returns a string representing the list separated by sep"
  [sep strlist]
  (apply str (interpose sep strlist)))

(defn decl-val
  "Takes a VAL declaration checks/infers the type and returns a mapping of symbol to type."
  [[form-type pat exp] env]
  (let [patvec (pat->vec pat env)
        tvec (type-of exp env)
        decl (str "val " (strlist->str " " patvec) " = " (->> exp
                                                              rest
                                                              (mapcat #(str (second %)))
                                                              (interpose " ")
                                                              (apply str)))]
    (check-patvec patvec tvec (atom env) decl)))

(defn decl-fun
  [[form-type [_ id] pat exp] env]
  (let [fn-type (type-of [:fn pat exp] env)]
    {id fn-type}))

(defn decl-id
  "Takes a valid declaration and returns a symbol and type
  pair. Returned pair to be stored in the current type-env."
  [[form body] env]
  {:pre (= :decl form)}
  (case (first body)
    :val (decl-val body env)
    :fun (decl-fun body env)))

(defn teval
  "Takes a ast and does the following:
  If it is a declaration => return a map of it's new environment.
  If it is an expression => return it's type."
  [ast env]
  (case (ast/tag-of ast)
    :expr (type-of ast env)
    :decl (decl-id ast env)))

(defn check-prog
  "Takes in a vector asts representing a program and type checks all forms."
  [asts]
  (let [env (atom {})]
    (doseq [ast asts]
      (let [res (teval ast @env)]
        (if (map? res)
          (swap! env conj res))))
    @env))

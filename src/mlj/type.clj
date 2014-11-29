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
            [mlj.lib :as lib]))

;; (declare type-of check-expr tuple?)
(declare unit?)
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
                                  :type-fn unit?}})
;;;;;;;;;;;;;;;;;;;;
;; Type Variables ;;
;;;;;;;;;;;;;;;;;;;;
;; (def type-vars (-> (map (comp keyword str char) (range (int \a) (int \z)))
;;                    vec
;;                    atom))
;; (defn type-var?
;;   [t]
;;   {:pre [(keyword? t)]}
;;   (and (not (*type-map* t))))

;;;;;;;;;;;;;;;;;
;; Tuple Types ;;
;;;;;;;;;;;;;;;;;
(defn tuple? [v] (vector? v))
(defn unit? [v] (and (tuple? v) (empty? v)))

;; (defn tuple-type
;;   [tuple env]
;;   {:pre [(tuple? tuple)]}
;;   (vec (map #(type-of % env) tuple)))

;; (defn tuple-repr
;;   [tuple]
;;   "Converts a tuple-type obj to it's external representation. [:int :int] to [:int * :int]"
;;   (vec (interpose '* tuple)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking primitives ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defn type? 
;;   "Returns true if t is a type form."
;;   [t]
;;   (if (tuple? t)
;;     (not-any? false? (map type? t))
;;     (contains? *type-map* t)))

;; (defn var-type
;;   "Gets the type of a var"
;;   [vr]
;;   (-> vr
;;       core/get-var
;;       meta
;;       :type))

;; (defn type-of 
;;   "Gets the type of a value"
;;   [v env]
;;   {:pre [(not (keyword? v)) (not (list? v))]}
;;   (cond
;;    (nil? v) :unit
;;    (tuple? v) (tuple-type v env)
;;    (var? v) (var-type v)
;;    (symbol? v) (env v)
;;    :else (-> (filter #((:type-fn (% 1)) v) *type-map*)
;;              first
;;              key)))

;; (defn check-type 
;;   "Check if v is of type t in env"
;;   [v t env]
;;   (= (type-of v env) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Type signatures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defn fn-type?
;;   "Checks if is a function type"
;;   [vr]
;;   (= :fn (core/typesig-type (var-type vr))))

;; (defn fn-type
;;   "Gets the type-signature of a function"
;;   [f]
;;   {:pre [(core/builtin? f)]}
;;   (core/get-type f))

;;;;;;;;;;;;;;;;;;;;;;;
;; Type Environments ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; (defn make-default-env
;;   "Create a default static environment {symbol type-sig}"
;;   []
;;   (reduce (fn [res sym]
;;             (assoc res sym (core/get-type sym)))
;;           {}
;;           (keys (core/op-map :builtin))))

;; ;;; atom should NOT be modified for lexical reasons, only modifications are top-level definitions
;; (def environment "Environment of {symbol type-sig}" (atom (make-default-env)))

;; (defn fn-env
;;   "Creates a lexical environment for a function"
;;   [params param-types]
;;   (if (symbol? params)
;;     (hash-map params param-types)
;;     (zipmap params param-types)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking forms ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Application should hold a type variable map, that maps a type variable to a type
;; (defn check-app
;;   "Type checks a function application. Returns the return type of the function if valid."
;;   [[f arg] env]
;;   (let [[par ret] (fn-type f)
;;         arg-types (if (tuple? arg)
;;                     (vec (map-indexed (fn [idx item] (if-let [t (check-expr item env)]
;;                                                        t
;;                                                        (let [sym (arg idx)
;;                                                              infered (par idx)]
;;                                                          (swap! env assoc sym infered)
;;                                                          infered)))
;;                                       arg))
;;                     (check-expr arg @env))]
;;     (if (not= arg-types par)
;;       (throw (IllegalArgumentException.
;;               (str "Type Error: " f " args: (" arg "), expecting: " par "\n Got: " arg-types)))
;;       ret)))

;; (defn check-if
;;   "Type checks an IF expression. Returns the type of the branches if valid, else throws an exception"
;;   [[if-sym pred then-sym t-expr else-sym e-expr :as if-expr] env]
;;   (do
;;     (if-let [pred-type (check-expr pred env)]
;;         (if (not= pred-type :bool)
;;           (throw (IllegalArgumentException.
;;                   (str "Type Error: test in IF expression not type bool"
;;                        "\n\ttest expression: " (type-of pred @env)
;;                        "\n\tin expression: " if-expr))))
;;         (swap! env assoc pred :bool))
;;     (let [t-type (check-expr t-expr env)
;;           e-type (check-expr e-expr env)
;;           t-type (if (and (nil? t-type) (not (nil? e-type)))
;;                    e-type t-type)
;;           e-type (if (and (nil? e-type) (not (nil? t-type)))
;;                    t-type e-type)]
;;       (if (symbol? t-expr) (swap! env assoc t-expr t-type))
;;       (if (symbol? e-expr) (swap! env assoc e-expr e-type))
;;       (if (= t-type e-type)
;;         t-type
;;         (throw (IllegalArgumentException.
;;                 (str "Type Error: IF branches types do not agree." t-type " <> " e-type)))))))
;; (defn- set-type! [sym t env]
;;   (do (swap! env assoc sym t)
;;       t))

;; (defn check-val
;;   "Type checks a VAL declaration"
;;   ([[val-sym sym & [& {tsig :- v :=}] :as val-expr] env]
;;      (let [t (check-expr v env)]
;;        (if (or (nil? tsig) (= tsig t))
;;          (do
;;            (swap! env assoc sym t)
;;            t)
;;          (throw (IllegalArgumentException.
;;                  (str "Type Error: VAL binding type mismatch."
;;                       "\n\tgot: " (type-of v @env)
;;                       "\n\tin: " (apply str (interpose \space val-expr)))))))))

;; (defn check-fn
;;   "Type checks a FN expression. Return the fn type if valid"
;;   [[fn-sym param & {[par-type ret-type :as tsig] :-, body :=>}] env]
;;   (let [env (atom (merge env (fn-env param par-type)))
;;         body-type (check-expr body env)
;;         par-type (if (nil? par-type) (check-expr param env) par-type)]
;;     (if (or (nil? ret-type) (= body-type ret-type)) 
;;       (list par-type body-type)
;;       (throw (IllegalArgumentException.
;;               (str "Type Error: FN return type mismatch."
;;                    "\n\tExpected: " ret-type
;;                    "\n\tGot: " body-type))))))

;; (defn check-fun
;;   "Type checks a FUN declaration. Return the fn type if valid"
;;   [[fun-sym name param & {[par-type ret-type :as tsig] :-, body :=}] env]
;;   (check-val ['val name :- tsig := (list 'fn param :- tsig :=> body)] env))

;; (defn check-let
;;   "Type check a LET expression. Returns the type of the body if valid"
;;   [[let-sym binding-exprs & {body :in} :as let-expr] env]
;;   (let [let-env (atom env)]
;;     (doseq [binding (keep-indexed #(if (odd? %1) (conj %2 'val))
;;                                   (partition-by #(= :val %) binding-exprs))]
;;       (check-val binding let-env))
;;     (check-expr body let-env)))

;; (defn check-expr
;;   "Type checks an expression. 
;;   Throws an exception if type is invalid, else returns the type of the expression"
;;   [expr env]
;;   (if (list? expr)
;;     (let [[form & body] expr]
;;       (cond
;;        (core/mlj-keyword? form) (case form
;;                                   val (check-val expr environment)
;;                                   if (check-if expr env)
;;                                   let (check-let expr @env)
;;                                   fn (check-fn expr @env)
;;                                   fun (check-fun expr environment)
;;                                   (throw (IllegalStateException. (str "Found uncheck keyword: " form))))
;;        (core/builtin? form) (check-app expr env)
;;        :else ((throw (IllegalStateException. (str "Found uncheck form: " form))))))
;;     (type-of expr @env)))


(declare decl-val create-env pat->vec)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti type-of "Takes a ast node and returns its type" (fn [node & _] (ast/tag-of node)))

(defmethod type-of :expr [[_ expr] env]
  (type-of expr env))

(defmethod type-of :int [_ env]
  :int)

(defmethod type-of :char [_ env]
  :char)

(defmethod type-of :string [_ env]
  :string)

(defmethod type-of :bool [_ env]
  :bool)

(defmethod type-of :tuple [[_ tuple] env]
  (mapv #(type-of % env) tuple))

(defmethod type-of :id [[_ sym] env]
  (get env sym))

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
        param-types (pat->vec param)
        fn-env (merge env (apply hash-map (flatten param-types)))
        ret-type (type-of body fn-env)]
    [:fn param-types ret-type]))

(defmethod type-of :default [_ env]
  (println _))

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
;;;;;;;;;;;;;;;;;
;; Expressions ;;
;;;;;;;;;;;;;;;;;
;; (defmethod check :expr [[_ exp & [ann]] env]
;;   (if (check exp env)
;;     (match-ann exp ann)
;;     (throw (ex-info "Type mismatch." {:received exp
;;                                       :expected ann}))))

;; (defmethod check :tuple [tuple env]
;;   (let [type-tuple (map #(check % env) tuple)]
;;     type-tuple))

;; (defmethod check :if [[_ test then else :as if-expr] env]
;;   (let [test-type (check test env)
;;         then-type (check then env)
;;         else-type (check else env)]
;;     (if (not= test-type :bool)
;;       (throw (ex-info "test expr in if is not type bool" {:test test-type
;;                                                           :expr if-expr}))
;;       (if (not= then-type else-type)
;;         (throw (ex-info "then and else expr in if type mismatch" {:then then-type
;;                                                                   :else else-type
;;                                                                   :expr if-expr}))
;;         then-type))))

;;;;;;;;;;;;;;;;;;
;; Declarations ;;
;;;;;;;;;;;;;;;;;;
(defn pat->vec
  "Takes a pattern node and converts it into a vector of pairs.
  The first index is the ids, and the second is the annotations."
  [[_ [pat-type & pat]]]
  (if (= pat-type :id)
    (vector (first pat) (if (> (count pat) 1) (parse-ann (second pat))))
    (mapv #(pat->vec %) pat)))

(defn decl-val
  [[form-type pat exp] env]
  ;; pairs ("x" :int, "y" :int)
  ;; actual (:int :int)
  (let [pairs (flatten (pat->vec pat))
        actual (vector (type-of exp env))]
    (loop [[id type & more-pairs] pairs
           [t & exp-types] actual
           result {}]
      (if (nil? t)
        result
        (if (not= t type)
          (throw (ex-info "Pattern and expression in val dec don't agree."
                                {:pattern type
                                 :expression t}))
          (recur more-pairs exp-types (conj result [id t])))))))

(defn decl-id
  "Takes a valid declaration and returns a symbol and type
  pair. Returned pair to be stored in the current type-env."
  [[form body] env]
  {:pre (= :decl form)}
  (decl-val body env))

;; (defmethod check :decl [[_ & [more]] env]
;;   (check more env))

;; (defmethod check :val [[_ [__ id & [ann]] expr] env]
;;   (match-ann id ann)
;;   (println id " " ann " " expr))

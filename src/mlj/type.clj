;;;; Sherman Pay

;;;; TODO: Change implementation of Types
;;;; TODO: Add Type variables
;;;; TODO: Catch errors in helper functions
(ns mlj.type
  "ML (Hindley Milner) type system.
  Includes a static type checker that type checks valid mlj forms.
  Also provides builtin mlj types."
  (:require [mlj.lang :as ml]
            [mlj.parser :as parser]
            [mlj.ast :as ast]
            [mlj.lib :as lib]))

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

;;;;;;;;;;;;;;;;;
;; Tuple Types ;;
;;;;;;;;;;;;;;;;;
(defn tuple? [v] (vector? v))
(defn unit? [v] (and (tuple? v) (empty? v)))

(declare decl-val create-env pat->vec patvec->tvec)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type checking helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti type-of "Takes a ast node and returns its type"
  (fn [node & _] (ast/tag-of node)))

(defmethod type-of :expr [[_ expr & more] env]
  (let [t (type-of expr env)]
    (if (and
         (vector? t)
         (list? :fn)
         (not= nil more))
      (last t)                          ; Function application
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
    (list (patvec->tvec param-types) ret-type)))

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
  "Takes a pattern node and converts it into a vector of pairs.
  The first index is the ids, and the second is the annotations."
  [[pat-type & pat]]
  (if (= pat-type :id)
    (list (first pat) (if (> (count pat) 1) (parse-ann (second pat))))
    (mapv #(pat->vec %) pat)))

(defn get-patvec
  [[_ [form-type pat exp]]]
  (pat->vec pat))

(defn patvec->tvec
  [patvec]
  (if (string? (first patvec))
    (second patvec)
    (mapv patvec->tvec patvec)))

(defn check-patvec
  [patvec tvec env]
  (if (vector? patvec)
    (do
      (doseq [[pair t] (map vector patvec tvec)]
        (swap! env conj (check-patvec pair t env)))
      @env)
    (let [[id pat-type] patvec]
     (if (and (not= pat-type tvec) (not= nil pat-type))
       (throw (ex-info "Pattern and expression in val dec don't agree."
                       {:pattern pat-type
                        :expression tvec}))
       (apply hash-map [id tvec])))))

(defn decl-val
  [[form-type pat exp] env]
  "Takes a VAL declaration checks/infers the type and returns a mapping of symbol to type."
  (let [patvec (pat->vec pat)
        tvec (type-of exp env)]
    (check-patvec patvec tvec (atom env))))

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

;; (defmethod check :decl [[_ & [more]] env]
;;   (check more env))

;; (defmethod check :val [[_ [__ id & [ann]] expr] env]
;;   (match-ann id ann)
;;   (println id " " ann " " expr))

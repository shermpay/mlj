(ns mlj.compiler
  "Compiles a set of MLJ forms into Clojure"
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.pprint :as pretty]
            [mlj.core :as core]
            [instaparse.core :as insta])
  (:gen-class))

(def parse
  (insta/parser
   "<program> = toplvl <';'>* ws* |  decl (ws* decl)* 
                | toplvl (ws* <';'>* ws* expr)*
    <toplvl> = decl | expr
    decl = val 
    val = <'val'> ws+ id ws* <'='> ws* expr

    expr =  exp ws* ann? | lp exp rp 
    <exp> = id | const | if | let | fn | id (ws* exp ws*)* 
    if = <'if'> ws+ expr ws+ <'then'> ws+ expr ws+ <'else'> ws+ expr ws*
    let = <'let'> ws+ (val ws)* <'in'> in <'end'> ws*
    in = ws+ expr ws+
    fn = <'fn'> ws+ id ws+ <'=>'> ws+ expr ws*

    <const> = number | bool | char | string | unit | tuple
    number = #'[0-9]+'
    bool = 'true' | 'false'
    char = <'#'> <'\\\"'> #'.' <'\\\"'>
    string = <'\\\"'> #'(\\\\.|[^\"])*' <'\\\"'>
    unit = lp ws* rp

    tuple = lp exp ws* (<','> ws* exp)+ rp
    ttuple = lp type ws* (<'*'> ws* type)+ rp

    ann = <':'> ws* (type | ttuple)
    type = 'int' | 'bool' | 'char' | 'string' | 'real' | 'unit'

    id = !reserved #'([a-zA-Z\\+\\-\\*/])+' ws* ann?
    <reserved> = bool | 'val' | 'if' | 'let' | 'fn' | type
    <ws> = <#'\\s+'>
    <lp> = <'('>
    <rp> = <')'>"))

(defn parse-res [filename & [pprint]]
  (-> filename
      io/resource
      slurp
      parse))

(defn tag-of [coll]
  (first coll))

(defn item-of [coll]
  (rest coll))

(defmulti compile "Takes a vector tree and compiles into a Clojure form" tag-of)

(defmethod compile :id [[_ item]]
  (symbol item))

(defmethod compile :number [[_ item]]
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

(defmethod compile :tuple [[_ & items]]
  (vec (map compile items)))

(defmethod compile :if [[_ & [pred then else]]]
  (list 'if
        (compile pred)
        (compile then)
        (compile else)))

(defmethod compile :let [[_ & exps]]
  (let [[bindings [[_ body]]] (partition-by #(= (tag-of %) :in) exps)]
    (list
     'let (->> bindings
               (map (fn [[_ id expr]]
                      (vector (compile id) (compile expr))))
               flatten
               vec)
     (compile body))))

(defmethod compile :fn [[_ params body]]
  (list 'fn
        (vector (compile params))
        (compile body)))

(defmethod compile :expr [[_ & items]]
  (if (= 1 (count items))
    (compile (first items))
    (map compile items)))

(defmethod compile :val [[_ & [sym exp]]]
  (list 'def
        (compile sym)
        (compile exp)))

(defmethod compile :decl [[_ item]]
  (compile item))

(defmethod compile :default [tree]
  (throw (ex-info "Unable to compile, malformed syntax" (into {} tree))))

(defn testing []
  (->> "comp.sml"
      parse-res
      (take 5)
     
      pretty/pprint))

;;; Type Signature parsing
(defn parse-fn-typesig
  " Takes in a Function type signature and parses it to return a
  function form Function types will be stored as '([tuple] return),
  list as function types.
  Example:
  :int = :int
  [:int * :int] = [:int :int]
  ([:int * :int] -> :int) = ([:int * :int] :int)
  ([:int * :int] -> :int -> :int) = ([:int * :int] (:int :int))
  ([:int * :int] -> :int -> :int -> :int) = ([:int * :int] (:int (:int :int))) "
  [tsig]
  (let [t (->> tsig
               (filter #(not= '-> %))
               reverse)]
    (reduce #(cons %2 (list %1)) t)))

(defn parse-typesig
  "Parse any form of type signature"
  [tsig]
  (case (core/typesig-type tsig)
    :prim tsig
    :tuple (vec (map parse-typesig
                     (filter #(not= '* %) tsig)))
    :fn (map parse-typesig
             (parse-fn-typesig tsig))
    (throw (NoSuchMethodException. "Illegal Type Syntax."))))

(defn parse-bindings
  [bindings]
  (map #(let [[val-sym sym tsig & more] %]
          (concat
           (list val-sym sym (parse-typesig tsig))
           more)) (partition 5 bindings)))

(let [b '[val x :int = 1
         val y [:int * :int] = [1, 2]]]
  (map #(nth % 2) (partition 5 b))) 
       
;;; Overall parsing
(comment
  (defmacro compile
   "Takes in the body a valid ML expression, parse it into a form
  recognizable by Clojure. Which is a Concrete Syntax Tree of Clojure
  macros defined in mlj.lang 
  Examples:
  (parse if pred then expr else expr) => (#'ml/if pred then expr else expr)"
   [& body]
   (loop [result []
          [hd & tl] body]
     (if (or (core/mlj-keyword? hd)
             (core/builtin? hd))
       (let [n (core/count-args hd)]
         (case hd
           val (recur (conj result
                            (conj (let [[sym tsig & a] `~(take n tl)]
                                    (concat (list sym (parse-typesig tsig)) a))
                                  (core/get-var hd)))
                      (nthrest tl n))

           fun (recur (conj result
                            (conj (let [[name param tsig & more] `~(take n tl)]
                                    (concat (list name param (parse-typesig tsig)) more))
                                  (core/get-var hd)))
                      (nthrest tl n))
           ;; Let case: take a let binding of the form `let val x [:int * :int] = [1, 2] in x end`
           ;; and transform it to `(let [x [:int :int] = [1 2]] in x end)`
           let (recur (conj result
                            (conj (let [expr `~(take-while #(not= 'end %) tl)
                                        [bindings body] (split-with #(not= % 'in) expr)]
                                    (concat (conj '() (vec (parse-bindings bindings))) body '(end)))
                                  (core/get-var hd)))
                      (nthrest tl n))

           (recur (conj result
                        (conj `~(take n tl)
                              (core/get-var hd)))
                  (nthrest tl n))))
       `(concat '() '~result)))))


(comment
  (compile if true then 1 else 2)
  (compile val x [:int * :int] = [1 2])
  (compile fun foo [x, y] ([:int * :int] -> :int) = 1)
  (compile let
           val x :int = 1
           val y [:int * :int] = [1, 2]
           in
           (+ x y)
           end))


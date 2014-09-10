(ns mlj.compiler
  "Compiles a set of MLJ forms into Clojure"
  (:refer-clojure :exclude [compile])
  (:require [mlj.core :as core])
  (:gen-class))

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
(defmacro parse
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
      `(concat '() '~result))))

(parse if true then 1 else 2)
(parse val x [:int * :int] = [1 2])
(parse fun foo [x, y] ([:int * :int] -> :int) = 1)
(parse let
       val x :int = 1
       val y [:int * :int] = [1, 2]
       in
       (+ x y)
       end)

(ns mlj.core
  "Core ML Syntax Macro forms."
  {:collect-only false}
  (:require [clojure.core :as c :reload true]
            [clojure.core.match :refer [match]]))

(alias 'ml 'mlj.core)

(defn ^:private substring
  "substring"
  ([^String st ^long i]
     (.substring st i))
  ([^String st ^long s ^long e]
     (.substring st s e)))

;;; Globally useful functions
(defn ^:private unqual-sym
  [sym]
  (c/let [s (str sym)]
   (symbol (substring s (inc (.indexOf s (int \/)))))))

;;; Type Syntax
(defn check-type [x t]
  "Check if x is of type t; returns true if typechecks."
  true)

(defmacro type-decl! [x t]
  "Declare x to be of type t"
  (if (check-type ~x ~t)
    (alter-meta! (var x) assoc :type '~t)))

;;; Syntax macros
(defmacro fn
  "Define a ML lambda.
  Example:
  fn x => x"
  [param ptype arrow & body]
  `(c/fn [~param]
     ~@(if (not= (unqual-sym arrow) (unqual-sym '=>))
         (throw (IllegalArgumentException.
               (str "Syntax Error: FUN requires =>; Got " arrow)))
         (do
           `(type-decl! '~param ~ptype)
           body))))

(defmacro if
  "ML if.
  Example: if pred then if-expr else else-expr"
  [pred then-sym if-expr else-sym else-expr]
  (cond (not= then-sym (unqual-sym 'then))
        (throw (IllegalArgumentException.
                (str "Syntax Error: IF expected THEN; Got " then-sym)))
        (not= else-sym (unqual-sym else-sym))
        (throw (IllegalArgumentException.
                (str "Syntax Error: IF expected ELSE; Got " else-sym)))
        :else `(if ~pred
                 ~if-expr
                 ~else-expr)))

(defmacro val
  "ML val keyword. Binds a symbol to a value."
  [sym eq-sym v]
  (if (not= (unqual-sym eq-sym) (unqual-sym '=))
    (throw (IllegalArgumentException.
            (str "Syntax Error: VAL expected =; Got " eq-sym)))
    `(def ~sym ~v)))

(defmacro fun
  "Define a ML named function."
  [name param eq-sym expr]
  `(val ~name ~eq-sym (ml/fn ~(unqual-sym param) => ~expr)))

;;; Helper functions for let
(defn ^:private let-pair?
  "Pair of bindings let. Takes in a vector of argument and its
  index. Returns true if it is a part of a let pair"
  [[i x]]
  (c/let [m (mod i 4)]
    (or (= m 1) (= m 3))))

(defn ^:private check-binding-syntax
  "Checks the let binding portion of the syntax"
  [v]
  (not-any? false?
            (map-indexed
                    #(cond
                      (even? %1) (= %2 (unqual-sym 'val))
                      (odd? %1) (= %2 (unqual-sym '=))
                      :else (throw (IllegalArgumentException. "index should be a number")))
            (keep-indexed #(if ((complement let-pair?) [%1 %2]) %2) v))))

(defn ^:private get-bindings
  "Get the binding components of a binding vector."
  [v]
  (into [] (keep-indexed #(if (let-pair? [%1 %2]) %2) v)))

(defmacro let
  "ML let keyword. Example: let bindings in expr end"
  [bindings in-sym expr end-sym]
  (cond
   (not (check-binding-syntax bindings))
   (throw (IllegalArgumentException.
           (str "Syntax Error: LET requires bindings of the form val symbol = value; Got "
                bindings)))
   (not= (unqual-sym in-sym) (unqual-sym 'in))
   (throw (IllegalArgumentException.
           (str "Syntax Error: LET requires IN keyword after bindings."
                bindings)))
   (not= (unqual-sym end-sym) (unqual-sym 'end))
   (throw (IllegalArgumentException.
           (str "Syntax Error: LET required END keyword at the close."
                bindings)))
   :else `(c/let ~(get-bindings bindings)
            ~expr)))

(defn ^:private check-pattern-exp
  "Check pattern matching expressions"
  [exprs]
 (not-any? false?
 (map-indexed
  #(cond (even? %1) (= (unqual-sym %2) (unqual-sym '=>))
         (odd? %1) (= (unqual-sym %2) (unqual-sym '|)))
  (keep-indexed #(if (odd? %1) %2) '(0 => 1 | 1 => 2 | _ => 3)))))

(defmacro case
  "ML case keyword for pattern matching.
  Example: 
        case x of 0 => 1 
                | 1 => 2 
                | _ => x"
  [m-exp of-sym & exprs]
  (cond (not= (unqual-sym of-sym) (unqual-sym 'of))
        (throw (IllegalArgumentException. (str "Syntax Error: CASE requires OF keyword.")))
        (check-pattern-exp exprs)
        (throw (IllegalArgumentException. (str "Syntax Error: Pattern invalid.")))
        :else `(match ~m-exp
           ~@exprs)))


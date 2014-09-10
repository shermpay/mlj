(ns mlj.lang
  "Provides the interface to mlj forms."
  {:collect-only false}
  (:refer-clojure :exclude [fn val let case])
  (:require [clojure.core :as c]
            [clojure.core.match :refer [match]]))

(alias 'ml 'mlj.lang)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globally useful functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ^:private substring
  "substring"
  ([^String st ^long i]
     (.substring st i))
  ([^String st ^long s ^long e]
     (.substring st s e)))

(defn ^:private unqual-sym
  [sym]
  (c/let [s (str sym)]
   (symbol (substring s (inc (.indexOf s (c/int \/)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lanauge Syntax Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:mlj fn
  "Define a ML lambda.
  Example:
  fn x => x"
  [param => & body]
  `(c/fn [~param]
     ~@(if (not= => '=>)
         (throw (NoSuchMethodException.
                 (str "Syntax Error: FN requires =>; Got " =>)))
         body)))

(defmacro ^:mlj if
  "ML if.
  Example: if pred then if-expr else else-expr"
  [pred then if-expr else else-expr]
  (cond (not= then 'then)
        (throw (NoSuchMethodException.
                (str "Syntax Error: IF expected THEN; Got " then)))
        (not= else 'else)
        (throw (NoSuchMethodException.
                (str "Syntax Error: IF expected ELSE; Got " else)))
        :else `(if ~pred
                 ~if-expr
                 ~else-expr)))


(defmacro ^:mlj val
  "ML val keyword. Binds a symbol to a value. t is the type declaration."
  [sym t = expr]
  (if (not= = '=)
    (throw (NoSuchMethodException.
            (str "Syntax Error: VAL expected =; Got " =)))
    `(do
       (def ~sym  ~expr)
       (alter-meta! #'~sym assoc :type '~t)
       '~sym)))

(defmacro ^:mlj fun
  "Define a ML named function. Type Signatures should be of form: ([:int * :int] -> :int)."
  [name param type-sig = expr]
  `(do
     (ml/val ~name ~type-sig ~= (ml/fn ~param ~'=> ~expr))
     (alter-meta! #'~name assoc :arglists '~(conj '() [param]))
     '~name))

;;; Helper functions for let
(defn ^:private let-pair?
  "Pair of bindings let. Takes in a vector of index and argument.
  Returns true if it is a part of a let pair"
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
                      :else (throw (NoSuchMethodException. "index should be a number")))
            (keep-indexed #(if ((complement let-pair?) [%1 %2]) %2) v))))

(defn ^:private get-bindings
  "Get the binding components of a binding vector."
  [v]
  (into [] (keep-indexed #(if (let-pair? [%1 %2]) %2) v)))

(defmacro ^:mlj ^:dynamic let
  "ML let keyword. Example: let bindings in expr end"
  [bindings in expr end]
  (c/let [bindings bindings]
    (cond
     (not (check-binding-syntax bindings))
     (throw (NoSuchMethodException.
             (str "Syntax Error: LET requires bindings of the form val symbol :T = value;" 
                  "Got " bindings)))
     (not= in 'in)
     (throw (NoSuchMethodException.
             (str "Syntax Error: LET requires IN keyword after bindings."
                  bindings)))
     (not= end 'end)
     (throw (NoSuchMethodException.
             (str "Syntax Error: LET required END keyword at the close."
                  bindings)))
     :else `(c/let ~(get-bindings bindings)
              ~expr))))

(defn ^:private check-pattern-exp
  "Check pattern matching expressions"
  [exprs]
 (not-any? false?
 (map-indexed
  #(cond (even? %1) (= (unqual-sym %2) (unqual-sym '=>))
         (odd? %1) (= (unqual-sym %2) (unqual-sym '|)))
  (keep-indexed #(if (odd? %1) %2) '(0 => 1 | 1 => 2 | _ => 3)))))

(defmacro ^:mlj case
  "ML case keyword for pattern matching.
  Example: 
        case x of 0 => 1 
                | 1 => 2 
                | _ => x"
  [m-exp of & exprs]
  (cond (not= unqual-sym of 'of)
        (throw (NoSuchMethodException. (str "Syntax Error: CASE requires OF keyword.")))
        (check-pattern-exp exprs)
        (throw (NoSuchMethodException. (str "Syntax Error: Pattern invalid.")))
        :else `(match ~m-exp
           ~@exprs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expose Language Meta Data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def keywords (into {}
                    (filter #(:mlj (meta (% 1)))
                            (ns-publics *ns*))))

(def syntax '#{then else in end = val})


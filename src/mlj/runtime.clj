(ns mlj.runtime
  (:require [mlj.type :as type])
  (:gen-class))

(defn ml-eval
  "Eval forms. If symbol holds a function, return its function signature;
  else return its value."
  [exp]
  (if (and (fn? exp) ((complement vector?) exp))
    (eval `(:type (meta (var ~exp))))
    (eval exp)))

(defn val->str
  [v]
  (cond
    (fn? v) "fn"
    (vector? v) (str "(" (apply str (interpose ", "(mapv val->str v))) ")")
    (list? v) (str "[" (apply str (interpose ", "(mapv val->str v))) "]")
    (string? v) (str \" v \")
    (char? v) (str "#\""  v \")
    :default v))

(defn type->str
  [t]
  (condp instance? t
    ;; Basic
    clojure.lang.Keyword (name t)
    ;; Tuple
    clojure.lang.PersistentVector (str "("
                                       (->> t
                                            (map type->str)
                                            (interpose " * ")
                                            (apply str))
                                       ")")
    ;; Fn
    mlj.type.FnType (let [par (.param t)
                          ret (.ret t)]
                      (str (type->str par)
                           " -> "
                           (type->str ret)))
    (throw (ex-info "Invalid type" {:type t}))))

(defn output-str [env v]
  "Given a Var and it's type environment. Converts it into a output string"
  (let [sym (:name (meta v))]
    (apply str
           (interpose " "
                      (list "val" sym "=" (val->str (var-get v))
                            ":" (type->str (env (name sym))))))))

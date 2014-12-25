(ns mlj.runtime
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
  (if (fn? v)
    "fn"
    v))

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
    clojure.lang.PersistentList (let [[par ret] t]
                                  (str (type->str par)
                                       " -> "
                                       (type->str ret)))
    (throw (ex-info "Invalid type" {:type t}))))

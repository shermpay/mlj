(ns mlj.compiler
  "Compiles a set of MLJ forms into Clojure"
  (:require [mlj.core :as core])
  (:gen-class))

(defmacro parse
  "Takes in the body a valid ML expression, parse it into a form
  recognizable by Clojure. Which is a Concrete Syntax Tree of Clojure
  macros defined in mlj.lang 
  Examples:
  (parse if pred then expr else expr) => (#'ml/if pred then expr else expr)"
  [& body]
  (loop [result []
         [hd & tl] body]
    (cond (or (core/mlj-keyword? hd)
              (core/mlj-fn? hd))
          (let [n (core/count-args hd)]
            (recur (conj result
                         (conj `~(take n tl)
                               (core/get-var hd)))
                   (nthrest tl n)))
          :else `(concat '() '~result))))

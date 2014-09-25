(ns mlj.ast
  "Abstract Syntax Tree functions"
  (:gen-class))

(defn tag-of [coll]
  (first coll))

(defn item-of [coll]
  (rest coll))

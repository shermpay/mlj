(ns mlj.core
  "Layer between mlj.lang and various components"
  (:require [mlj.lang :as ml]
            [mlj.lib :as lib]))

(def op-map
  "Map for various mlj forms.
  :builtin Builtin function
  :keyword Syntax keyword"
  {:builtin lib/builtins
   :keyword ml/keywords})

(defn mlj-var
  "Gets the var holding the keyword/function. "
  [k sym]
  (get (k op-map) sym))

(defn mlj-keyword?
  "Checks if sym is a mlj keyword"
  [sym]
  (boolean (:mlj (meta (mlj-var :keyword sym)))))

(defn mlj-fn?
  "Checks if sym is a builtin mlj function"
  [sym]
  (boolean (mlj-var :builtin sym)))

(defn form-type
  "Obtain the language form type."
  [sym]
  (cond (mlj-keyword? sym) :keyword
        (mlj-fn? sym) :builtin
        :else (throw (UnsupportedOperationException. "form-type: Unknown language construct"))))

(defn get-var
  "Gets the var representing the symbol."
  [sym]
  (let [op (form-type sym)]
    (mlj-var op sym)))

(defn get-args
  "Gets the args of a mlj keyword/function"
  [k]
  (-> k
      get-var 
      meta
      :arglists
      first))
(defn count-args
  "Gets the count of args of a mlj keyword/function"
  [k]
  (count (get-args k)))
;; (defn syntax-pos
;;   [k syntax]
;;   (.indexOf (count-args k) syntax))

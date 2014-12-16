(ns mlj.repl
  "REPL for MLJ lang"
  (:require [mlj.compiler :as compiler]
            [mlj.parser :as parser]
            [mlj.type :as type]))

(defn intro
  "Prints an intro message."
  []
  (println "MLJ interpreter")
  (println "Type exit to quit."))

(def ^:dynamic *prompt* "- ")
(def ^:dynamic *output* ">")

(defn ml-eval
  "Eval forms. If symbol hold function return it's function signature,
  else return it's value."
  [exp]
  (if (and (ifn? exp) ((complement vector?) exp))
    (eval `(:type (meta (var ~exp))))
    (eval exp)))

(defn handle-decl
  "Takes a declaration outputs to stdout and returns it's environment"
  [decl env]
  (let [new-env (type/decl-id decl env)
        v (eval (compiler/compile decl))]
    (doseq [[sym t] new-env]
      (println *output* "val" sym "=" (var-get v) t))
    new-env))

(defn handle-expr
  [expr env]
  (let [t (type/type-of expr env)]
    (try
      (println *output* "val it =" (ml-eval (compiler/compile expr)) t)
      (catch java.lang.RuntimeException e
        (println *output* "Error: unbound variable:" (last (last expr)))))))

(defn eval-print
  "Eval an input string and print the corresponding results."
  [input env]
  (let [parse-tree (->> input (parser/parse) (first))]
    (try
      (case (first parse-tree)
       :decl (handle-decl parse-tree env)
       :expr (handle-expr parse-tree env))
      (catch clojure.lang.ExceptionInfo e
        (println (.getMessage e) (ex-data e)))))) 

(defn prompt-read
  "Prints a prompt and reads a string of input."
  []
  (print *prompt*)
  (flush)
  (read-line))

(defn main []
  (intro)
  (refer 'mlj.lib)
  (loop [env (mlj.lib/types)
         input (prompt-read)]
    (if (or (= input "exit") (nil? input))
      (println "Exiting MLJ.")
      (recur (conj env (eval-print input env))
             (prompt-read)))))

(ns mlj.main
  "Main entry point"
  (:require [mlj.runtime :as runtime]
            [mlj.repl :as repl]
            [mlj.type :as type]
            [mlj.compiler :as compiler]
            [mlj.parser :as parser])
  (:gen-class))

(def flags {:h "Prints this help message."})

(defn key->flag
  "Takes a key and transforms into a flag. eg. :k -> -k"
  [k]
  (str "-" (name k)))

(defn usage
  "Create and return usage string"
  []
  (println "Usage: mlj [option] filename\n")
  (doseq [[k v] flags]
    (str "  " (key->flag k) "\t" v)))

(defn parse-str
  [string]
  (let [parse-res (parser/parse string)]
    (if (parser/parse-error? parse-res)
      (throw (ex-info "Parse Error." {:type :parse-err
                                      :details parse-res}))
      parse-res)))

(defn name-var->str
  "Convert a name and var to string. eg. x #'x -> val x = 1"
  [name var]
  (let [v (var-get var)
        t (:type v)]
   (str "val " name " = " v " " t)))

(defn ns-map->val-str
  "Take a namespace map {symbol var} and a type environemtn; transforms that into
  a string of val declarations"
  [m env]
  (->> m
       (reduce-kv (fn [accum name var] (conj accum (name-var->str name var))) [])
       (interpose "\n")
       (apply str)))

(defn create-module
  "Creates a namespace that represents a module based on path to file."
  [path]
  (let [basename (last (clojure.string/split path #"/"))
        prefix (subs basename 0 (.indexOf basename "."))]
    (create-ns (symbol prefix))))

(defn compile-with-module
  "Compile a vector of program asts under a module"
  [asts module]
  (binding [*ns* module]
    (refer 'mlj.lib)
    (try
      (compiler/compile-prog asts)
      (catch clojure.lang.ExceptionInfo e
        (print (:details (ex-data e)))))))

(defn compile-run-file
  "Compiles and evaluates a file relative to project dir"
  [filename]
  (let [module (create-module filename)
        prog-str (slurp filename)
        asts (parse-str prog-str)
        env (type/check-prog asts)
        prog (compile-with-module asts module)]
    (binding [*ns* module]
      (clojure.core/refer-clojure)
      (doseq [p prog]
        (let [v (runtime/ml-eval p)]
          (if (vector? v)
            (doseq [s (map (partial runtime/output-str env) v)]
              (println s))
            (println (runtime/output-str env v))))))))

(defn -main [& args]
  (case (count args)
    0 (repl/main)
    1 (compile-run-file (first args))
    (println (usage))))

(ns mlj.main
  "Main entry point"
  (:require [mlj.repl :as repl]
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

(defn compile-str
  "Compile a string. Throws ex-info if syntax error"
  [string]
  (let [parse-res (parser/parse string)]
    (if (parser/parse-error? parse-res)
      (throw (ex-info "Parse Error." {:type :parse-err
                                      :details parse-res}))
      (compiler/compile-prog parse-res))))

(defn name-var->str
  "Convert a name and var to string. eg. x #'x -> val x = 1"
  [name var]
  (str "val " name " = " (var-get var)))

(defn ns-map->val-str
  "Take a namespace map {symbol var} and transform it into
  a string of val declarations"
  [m]
  (->> m
       (reduce-kv (fn [accum name var] (conj accum (name-var->str name var))) [])
       (interpose "\n")
       (apply str)))

(defn compile-run
  "Compile and run a file given its path."
  [path]
  (let [basename (last (clojure.string/split path #"/"))
        prefix (subs basename 0 (.indexOf basename "."))
        module (create-ns (symbol prefix))]
    (binding [*ns* module]
      (time
       (clojure.core/refer-clojure))
      (refer 'mlj.lib)
      (doseq [expr (try
                     (compile-str (slurp path))
                     (catch clojure.lang.ExceptionInfo e
                       (print (:details (ex-data e)))))]
        (eval expr))
      (ns-publics module))))

(defn compile-run-file [filename]
  (-> filename
      (compile-run)
      (ns-map->val-str)
      (println)))

(defn -main [& args]
  (case (count args)
    0 (repl/main)
    1 (compile-run-file (first args))
    (println (usage))))

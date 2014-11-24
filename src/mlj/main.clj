(ns mlj.main
  "Main entry point"
  (:require [mlj.repl :as repl]
            [mlj.compiler :as compiler]
            [mlj.parser :as parser])
  (:gen-class))

(def flags {:h "Prints this help message."})

(defn key->flag [k]
  (str "-" (name k)))

(defn usage []
  (println "Usage: mlj [option] filename\n")
  (doseq [[k v] flags]
    (println "  " (key->flag k) "\t" v)))

(defn compile-str [string]
  (compiler/compile-prog
   (parser/parse string)))

(defn name-var->str [name var]
  (str "val " name " = " (var-get var)))

(defn ns-map->val-str [m]
  (->> m
       (reduce-kv (fn [accum name var] (conj accum (name-var->str name var))) [])
       (interpose "\n")
       (apply str)))

(defn compile-run [path]
  (let [basename (last (clojure.string/split path #"/"))
        prefix (subs basename 0 (.indexOf basename "."))
        module (symbol prefix)]
    (in-ns module)
    (doseq [expr (compile-str (slurp path))]
      (eval expr))
    (ns-publics module)))

(defn -main [& args]
  (case (count args)
    0 (repl/main)
    1 (-> (first args)
          (compile-run)
          (ns-map->val-str)
          (println))
    (println (usage))))

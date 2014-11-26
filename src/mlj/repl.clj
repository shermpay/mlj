(ns mlj.repl
  "REPL for MLJ lang"
  (:require [mlj.compiler :as compiler]
            [mlj.parser :as parser]))

(defn intro
  "Prints an intro message."
  []
  (println "MLJ interpreter")
  (println "Type exit to quit."))

(def ^:dynamic *prompt* "- ")
(def ^:dynamic *output* "=")

(defn ml-eval
  "Eval forms. If symbol hold function return it's function signature,
  else return it's value."
  [exp]
  (if (ifn? exp)
    (eval `(meta (var ~exp)))
    (eval exp)))

(defn eval-print
  "Eval and print an input string."
  [input]
  (->> input
       (parser/parse)
       (first)
       (compiler/compile)
       (ml-eval)
       (println *output*)))

(defn prompt-read
  "Prints a prompt and reads a string of input."
  []
  (print *prompt*)
  (flush)
  (read-line))

(defn main []
  (intro)
  (refer 'mlj.lib)
  (loop [input (prompt-read)]
    (if (or (= input "exit") (nil? input))
      (println "Exiting MLJ.")
      (do (eval-print input)
          (recur (prompt-read))))))

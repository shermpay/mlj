(ns mlj.repl
  "REPL for MLJ lang"
  (:require [mlj.compiler :as compiler]
            [mlj.parser :as parser]))

(defn intro []
  (println "MLJ interpreter")
  (println "Type exit to quit"))

(def ^:dynamic *prompt* "- ")
(def ^:dynamic *output* "=")

(defn eval-print [input]
  (->> input
      (parser/parse)
      (first)
      (compiler/compile)
      (eval)
      (println *output*)))

(defn prompt-read []
  (print *prompt*)
  (flush)
  (read-line))

(defn main []
  (intro)
  (loop [input (prompt-read)]
    (if (or (= input "exit") (nil? input))
      (println "Exiting MLJ.")
      (do (eval-print input)
          (recur (prompt-read))))))

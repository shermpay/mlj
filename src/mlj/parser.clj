(ns mlj.parser
  "Builds a parse tree."
  (:require [clojure.java.io :as io]
            [mlj.core :as core]
            [mlj.compiler :as mljc]
            [instaparse.core :as insta])
  (:gen-class))

(def parse
  (insta/parser
   "<program> = toplvl <';'>* ws* |  decl (ws* decl)* 
                | toplvl (ws* <';'>* ws* expr)*
    <toplvl> = decl | expr
    decl = val 
    val = <'val'> ws+ id ws* <'='> ws* expr

    expr =  exp ws* ann? | lp exp rp 
    <exp> = id | const | if | let | fn | id (ws* exp ws*)* 
    if = <'if'> ws+ expr ws+ <'then'> ws+ expr ws+ <'else'> ws+ expr ws*
    let = <'let'> ws+ (val ws)* <'in'> in <'end'> ws*
    in = ws+ expr ws+
    fn = <'fn'> ws+ id ws+ <'=>'> ws+ expr ws*

    <const> = int | bool | char | string | unit | tuple
    int = #'[0-9]+'
    bool = 'true' | 'false'
    char = <'#'> <'\\\"'> #'.' <'\\\"'>
    string = <'\\\"'> #'(\\\\.|[^\"])*' <'\\\"'>
    unit = lp ws* rp

    tuple = lp exp ws* (<','> ws* exp)+ rp
    ttuple = lp type ws* (<'*'> ws* type)+ rp

    ann = <':'> ws* (type | ttuple)
    type = 'int' | 'bool' | 'char' | 'string' | 'real' | 'unit'

    id = !reserved #'([a-zA-Z\\+\\-\\*/])+' ws* ann?
    <reserved> = bool | 'val' | 'if' | 'let' | 'fn' | type
    <ws> = <#'\\s+'>
    <lp> = <'('>
    <rp> = <')'>"))

(def parse-error? insta/failure?)

(defn parse-res [filename & [pprint]]
  (-> filename
      io/resource
      slurp
      parse))


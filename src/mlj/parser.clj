;;; Sherman Pay Jing Hao
;;; Wednesday, 26. November 2014
;;; TODO: Parse Error Handling using Total Parse Mode
(ns mlj.parser
  "Parse MLJ forms into valid Clojure parse trees.
  Parse trees are nested vectors with a 'tag' representing the type of the node.
  Use parse-error? to validate the parsed result before using it."
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta])
  (:gen-class))

(def parse
  "Parse MLJ forms. Returns a parse tree where each node is a vector
  of the form ['tag' value], where 'tag' is a Clojure keyword."
  (insta/parser
   "<program> = toplvl <';'>* ws* |  decl (ws* decl)* 
                | toplvl (ws* <';'>* ws* expr)*
    <toplvl> = decl | expr
    decl = val | fun
    val = <'val'> ws+ pat ws* <'='> ws* expr
    fun = <'fun'> ws+ id pat+ ws* <'='> ws* expr

    expr =  exp ws* ann? | lp exp rp 
    <exp> = id | const | if | let | fn | id (ws* exp ws*)* | tuple
    if = <'if'> ws+ expr ws+ <'then'> ws+ expr ws+ <'else'> ws+ expr ws*
    let = <'let'> ws+ (val ws)* <'in'> in <'end'> ws*
    in = ws+ expr ws+
    fn = <'fn'> ws+ pat ws+ <'=>'> ws+ expr ws*

    <const> = int | bool | char | string | unit 
    int = #'[0-9]+'
    bool = 'true' | 'false'
    char = <'#'> <'\\\"'> #'.' <'\\\"'>
    string = <'\\\"'> #'(\\\\.|[^\"])*' <'\\\"'>
    unit = lp ws* rp

    tuple = lp expr ws* (<','> ws* expr)+ rp
    pattuple = lp pat ws* (<','> ws* pat)+ rp
    ttuple = lp type ws* (<'*'> ws* type)+ rp

    <pat> = const | blank | id | lp pat rp | pattuple
    ann = <':'> ws* (type | ttuple)
    type = 'int' | 'bool' | 'char' | 'string' | 'real' | 'unit'

    blank = '_'
    id = !(reserved ws+) #'([a-zA-Z0-9_\\+\\-\\*/])+' ws* ann?
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


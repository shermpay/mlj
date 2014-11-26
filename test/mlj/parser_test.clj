(ns mlj.parser-test
  (:require [clojure.test :refer :all]
            [mlj.parser :as parser]))

(deftest const-test
  (testing "Constants => int, char, string, bool. Basic cases."
    (is (= (parser/parse "1") '([:expr [:int "1"]])) "int")
    (is (not= (parser/parse "1.0") '([:expr [:float "1"]])) "float: unsupported")

    (is (= (parser/parse "#\"a\"") '([:expr [:char "a"]])) "char")

    (is (= (parser/parse "true") '([:expr [:bool "true"]])) "bool: true")
    (is (= (parser/parse "false") '([:expr [:bool "false"]])) "bool: false")

    (is (= (parser/parse "\"hello\"") '([:expr [:string "hello"]]))
        "string: simple")
    (is (= (parser/parse "\"hello world\"") '([:expr [:string "hello world"]]))
        "string: with space")))

(deftest num-test
  (testing "Numbers => int, float(unsupported)"
    (is (= (parser/parse "10") '([:expr [:int "10"]])) "2 digits")
    (is (= (parser/parse "100") '([:expr [:int "100"]])) "3 digits")
    (is (= (parser/parse "10000") '([:expr [:int "10000"]])) "5 digits")
    (is (= (parser/parse "1000000") '([:expr [:int "1000000"]])) "7 digits")
    (is (= (parser/parse "10000000000") '([:expr [:int "10000000000"]])) "11 digits")
    (is (= (parser/parse "1000000000000") '([:expr [:int "1000000000000"]])) "13 digits")
    (is (= (parser/parse "001") '([:expr [:int "001"]])) "prefix 0")
    (is (not= (parser/parse "a1") '([:expr [:int "1"]])) "invalid int: a1")
    (is (not= (parser/parse "a1") '([:expr [:int "a1"]])) "invalid int: a1")
    (is (not= (parser/parse "1a") '([:expr [:int "1a"]])) "invalid int: 1a")
    (is (not= (parser/parse "1a") '([:expr [:int "1"]])) "invalid int: 1a")))

(deftest char-test
  (testing "Char: subject to change"
    (is (not= (parser/parse "#\"ab\"") '([:expr [:char "a"]])) "char: invalid")
    (is (= (parser/parse "#\"A\"") '([:expr [:char "A"]])) "char: capitalized")
    (is (= (parser/parse "#\"!\"") '([:expr [:char "!"]])) "char: !")
    (is (= (parser/parse "#\"@\"") '([:expr [:char "@"]])) "char: @")
    (is (= (parser/parse "#\"#\"") '([:expr [:char "#"]])) "char: #")
    (is (= (parser/parse "#\"$\"") '([:expr [:char "$"]])) "char: $")
    (is (= (parser/parse "#\"%\"") '([:expr [:char "%"]])) "char: %")
    (is (= (parser/parse "#\"^\"") '([:expr [:char "^"]])) "char: ^")
    (is (= (parser/parse "#\"&\"") '([:expr [:char "&"]])) "char: &")
    (is (= (parser/parse "#\"*\"") '([:expr [:char "*"]])) "char: *")
    (is (= (parser/parse "#\"(\"") '([:expr [:char "("]])) "char: (")
    (is (= (parser/parse "#\")\"") '([:expr [:char ")"]])) "char: )")
    (is (= (parser/parse "#\"-\"") '([:expr [:char "-"]])) "char: -")
    (is (= (parser/parse "#\"_\"") '([:expr [:char "_"]])) "char: _")
    (is (= (parser/parse "#\"=\"") '([:expr [:char "="]])) "char: =")
    (is (= (parser/parse "#\"+\"") '([:expr [:char "+"]])) "char: +")
    (is (= (parser/parse "#\"|\"") '([:expr [:char "|"]])) "char: |")
    (is (= (parser/parse "#\"\\\"") '([:expr [:char "\\"]])) "char: \\")
    (is (= (parser/parse "#\":\"") '([:expr [:char ":"]])) "char: :")
    (is (= (parser/parse "#\";\"") '([:expr [:char ";"]])) "char: ;")
    (is (= (parser/parse "#\"'\"") '([:expr [:char "'"]])) "char: '")
    (is (= (parser/parse "#\"\"\"") '([:expr [:char "\""]])) "char: \"")
    (is (= (parser/parse "#\",\"") '([:expr [:char ","]])) "char: ,")
    (is (= (parser/parse "#\".\"") '([:expr [:char "."]])) "char: .")
    (is (= (parser/parse "#\".\"") '([:expr [:char "."]])) "char: .")
    (is (= (parser/parse "#\"?\"") '([:expr [:char "?"]])) "char: ?")))

(deftest bool-test
  (testing "Booleans"
    (is (not= (parser/parse "falselol") '([:expr [:bool "false"]])) "bool: invalid prefix")
    (is (not= (parser/parse "lolfalse") '([:expr [:bool "false"]])) "bool: invalid suffix")
    (is (not= (parser/parse "truefalse") '([:expr [:bool "truefalse"]])) "bool: combined")
    (is (not= (parser/parse "t") '([:expr [:bool "t"]])) "bool: combined")))

(deftest string-test
  (testing "Strings"
    (is (= (parser/parse "\"h\\\"")) '([:expr [:string "h\\"]]))
    (is (= (parser/parse "\"h\\\\\"")) '([:expr [:string "h\\\\"]]))
    (is (= (parser/parse "\"\\h\\\"")) '([:expr [:string "\\h\\"]]))
    (is (= (parser/parse "\"\n\"")) '([:expr [:string "\n"]]))
    (is (= (parser/parse "\"\t\"")) '([:expr [:string "\t"]]))
    (is (= (parser/parse "\"\"\"")) '([:expr [:string "\""]]))))

(deftest if-test
  (testing "if expression"
    (is (= (parser/parse "if true then 1 else false"))
        '([:expr [:if [:expr [:bool "true"]]
                  [:expr [:int "1"]]
                  [:expr [:int "2"]]]])  "simple if")
    (is (= (parser/parse "if if true then true else false then 1 else 2"))
        '([:expr [:if [:expr
                       [:if [:expr [:bool "true"]]
                        [:expr [:bool "true"]]
                        [:expr [:bool "false"]]]]
                  [:expr [:int "1"]]
                  [:expr [:int "2"]]]])
        "if: nested predicate")))

(deftest let-test
  (testing "let expression"
    (is (= (parser/parse "let val x = 1 in x end"))
        '([:expr [:let [:val [:pat [:id "x"]] [:expr [:int "1"]]]
                  [:in [:expr [:id "x"]]]]])  "simple let")))

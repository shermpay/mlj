(ns mlj.type-test
  (:refer-clojure)
  (:require [clojure.test :refer :all]
            [mlj.type :refer :all]))

;; (deftest tuple-test
;;   "Tests for primitive tuple operations"
;;   (is (tuple? [1 2]))
;;   (is (unit? []))
;;   (is (= (tuple-type [1 \a "b"] {}) [:int :char :string]))
;;   (is (= (tuple-repr [:int :char]) [:int '* :char])))

(deftest typechecking-primitives-test
  "Test for type checking primitives"
  (are [x p] (= (type? x) p)
       :int true
       :bool true
       :string true
       :char true
       [:int :char] true
       1 false
       [:int 1] false)

  (is (= (var-type '+) '([:int :int] :int)))

  (are [v e t] (= (type-of v e) t)
       1 {} :int
       'x {'x :int} :int)

  (are [v t e p] (= (check-type v t e) p)
       1 :int {} true
       'x :int {'x :int} true
       'x :int {} false)

  (are [v1 v2 e p] (= (same-type? v1 v2 e) p)
       1 1 {} true
       'x 'y {'x :int,'y :int} true
       'x 'y {'x :int, 'y :char} false
       'x 'y {'x :int} false
       'x 'y {} false))

(deftest fn-type-test
  (is (fn-type? '+))
  (is (= (fn-type '+) '([:int :int] :int))))

(deftest environment-test
  (is (map? (make-default-env)))
  (is (every? #{'+ '- '* '/} (keys (make-default-env))))
  (is (= (fn-env '[x y] [:int :char]) {'x :int, 'y :char})))

;;; Type checking all forms
(deftest check-app-test
  (is (= (check-app '(+ [1 2]) (atom {})) :int))
  (is (thrown? IllegalArgumentException (check-app '(+ [1 "1"]) (atom {})) :int)))

(deftest check-val-test
  (let [env (atom {})]
    (is (= (check-val '(val x :- :int := 1) env) :int))
    (is (thrown? IllegalArgumentException (check-val '(val y :- :int := "1") env)))
    (is (= (@env 'x) :int))
    (is (nil? (@env 'y)))))

(deftest check-if-test
  (is (= (check-if '(if true then 1 else 2) (atom {})) :int))
  (is (thrown? IllegalArgumentException (check-if '(if true then 1 else "2") (atom {}))))
  (is (thrown? IllegalArgumentException (check-if '(if 1 then 1 else 2) (atom {})))))

(deftest check-let-test
  (is (= (check-let '(let [:val x :- :int := 1] :in x) {}) :int))
  (is (= (check-let '(let [:val x :- :int := 1
                           :val y :- :int := x] :in y) {}) :int))
  (is (= (check-let '(let [:val x :- :int := 1
                           :val y :- :int := 2] :in (+ [x y])) {}) :int))
  (is (= (check-let '(let [:val x :- :int := 1
                           :val y :- :char := \a] :in y) {}) :char))
  (is (= (check-let '(let [] :in 1) {}) :int))
  (is (thrown? IllegalArgumentException
               (check-let '(let [:val x :- :int := \a] :in x) {})))
  (is (thrown? IllegalArgumentException 
               (check-let '(let [:val x :- :int := 1
                                 :val y :- :char := x] :in x) {})))

  (is (= (check-let '(let [:val x := 1
                           :val y := x] :in y) {}) :int) "Infered Type")
  (is (thrown? IllegalArgumentException
       (check-let '(let [:val x := 1
                         :val y :char := x] :in y) {})) "Infered Type with error")
  (is (= (check-let '(let [:val x := 1]
                       :in
                       (let [:val y := x]
                         :in
                         y)) {})
         :int) "Nested inference"))

(deftest check-fn-test
  (is (= (check-fn '(fn x :- (:int :int) :=> 1) {}) '(:int :int)))
  (is (= (check-fn '(fn [x y] :=> (+ [x y])) {}) '([:int :int] :int)) "App inference")
  (is (= (check-fn '(fn [x y] :=> (if x then 1 else y)) {})
         '([:bool :int] :int)) "If inference")
  (is (= (check-fn '(fn x :=> (if (if true then x else true) then 1 else 2)) {})
         '(:bool :int))))

(deftest check-fun-test
  (is (= (check-fun '(fun foo x :- (:int :int) := 1) (atom {})) '(:int :int)))
  (is (= (check-fun '(fun bar [x y] := (+ [x y])) (atom {})) '([:int :int] :int)) "App inference")
  (is (= (check-fun '(fun baz [x y] := (if x then 1 else y)) (atom {})) '([:bool :int] :int)) "If inference"))











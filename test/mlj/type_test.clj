(ns mlj.type-test
  (:refer-clojure)
  (:require [clojure.test :refer :all]
            [mlj.type :refer :all]))

(deftest tuple-test
  "Tests for primitive tuple operations"
  (is (tuple? [1 2]))
  (is (unit? []))
  (is (= (tuple-type [1 \a "b"] {}) [:int :char :string]))
  (is (= (tuple-repr [:int :char]) [:int '* :char])))

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
  (is (= (check-app '(+ [1 2]) {}) :int))
  (is (thrown? IllegalArgumentException (check-app '(+ [1 "1"]) {}) :int)))

(deftest check-val-test
  (let [env (atom {})]
    (is (= (check-val '(val x :int = 1) env) :int))
    (is (thrown? IllegalArgumentException (check-val '(val y :int = "1") env)))
    (is (= (@env 'x) :int))
    (is (nil? (@env 'y)))))

(deftest check-if-test
  (is (= (check-if '(if true then 1 else 2) {}) :int))
  (is (thrown? IllegalArgumentException (check-if '(if true then 1 else "2"))))
  (is (thrown? IllegalArgumentException (check-if '(if 1 then 1 else 2)))))

(deftest check-let-test
  (is (= (check-let '(let [val x :int = 1] in x end) {}) :int))
  (is (= (check-let '(let [val x :int = 1
                           val y :int = x] in y end) {}) :int))
  (is (= (check-let '(let [val x :int = 1
                           val y :int = 2] in (+ [x y]) end) {}) :int))
  (is (= (check-let '(let [val x :int = 1
                           val y :char = \a] in y end) {}) :char))
  (is (= (check-let '(let [] in 1 end) {}) :int))
  (is (thrown? IllegalArgumentException
               (check-let '(let [val x :int = \a] in x end) {})))
  (is (thrown? IllegalArgumentException
               (check-let '(let [val x :int = 1
                                 val y :char = x] in x end) {}))))

(deftest check-fn-test
  (is (= (check-fn '(fn x (:int :int) => 1) {}) '(:int :int))))

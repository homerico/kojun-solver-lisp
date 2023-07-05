(defpackage kojun-solver-lisp/tests/main
  (:use :cl
        :kojun-solver-lisp
        :rove))
(in-package :kojun-solver-lisp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :kojun-solver-lisp)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

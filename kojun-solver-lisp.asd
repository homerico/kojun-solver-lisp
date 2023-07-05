(defsystem "kojun-solver-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:iterate :cl :screamer)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "kojun-solver-lisp/tests"))))

(defsystem "kojun-solver-lisp/tests"
  :author ""
  :license ""
  :depends-on ("kojun-solver-lisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for kojun-solver-lisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))

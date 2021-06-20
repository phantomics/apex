;;;; System definition for MaxPC test and benchmark suite.

#+asdf3 (in-package :asdf-user)

(defsystem maxpc-apache
  :description
  "Test and benchmark suite for MaxPC."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "Apache-2.0"
  :components ((:file "test")
               (:file "bench")
               (:file "example-sexp"))
  :depends-on ("maxpc-apache")
  :perform (test-op (o s) #+asdf3 (uiop:symbol-call :maxpc.test :run-tests)))

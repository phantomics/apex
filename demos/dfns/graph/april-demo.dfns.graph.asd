;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Cnn -*-
;;;; april-demo.dfns.graph.asd

(asdf:defsystem #:april-demo.dfns.graph
  :description "Demo of April used to implement Dyalog graph functions"
  :author "Andrew Sengul"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ("april")
  :components ((:file "package")
	       (:file "setup")
               (:file "demo")))

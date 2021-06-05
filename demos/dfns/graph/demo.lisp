;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Dfns.Graph -*-
;;;; demo.lisp

(in-package :april-demo.dfns.graph)

(april-load (with (:space graph-demo-space))
	    (asdf:system-relative-pathname (intern (package-name *package*) "KEYWORD") "graph.apl"))

;; http://dfns.dyalog.com/s_path.htm

;; Directed graph functions.

(specify-demo
 "April graph demo"
 (with :space graph-demo-space
       :description "Implements graph processing functions from Dyalog's dfns.")
 (:tests (provision "g←(2 3) (3) (2 4) (1 5) (3)")
	 (is "g path 2 1 " #(2 3 4 1))
	 (is "g path (1 2)(4 5)" #(2 3 4))
	 (is "g∘path¨⍳5 5" #2A((#(1) #(1 2) #(1 3) #(1 3 4) #(1 3 4 5))
			       (#(2 3 4 1) #(2) #(2 3) #(2 3 4) #(2 3 4 5))
			       (#(3 4 1) #(3 2) #(3) #(3 4) #(3 4 5))
			       (#(4 1) #(4 1 2) #(4 5 3) #(4) #(4 5))
			       (#(5 3 4 1) #(5 3 2) #(5 3) #(5 3 4) #(5))))))

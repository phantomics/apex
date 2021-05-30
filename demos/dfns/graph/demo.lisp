;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Cnn -*-
;;;; demo.lisp

(in-package :april-demo.dfns.graph)

(april-load (with (:space graph-demo-space))
	    (asdf:system-relative-pathname (intern (package-name *package*) "KEYWORD") "graph.apl"))

;; http://dfns.dyalog.com/s_path.htm

;; directed graph.
(defun run-tests ()
  (setq prove:*enable-colors* nil)
  (plan 4)

  (princ "Tests for April graph demo.")
  
  (april (with (:space graph-demo-space))
	 "g←(2 3) (3) (2 4) (1 5) (3)")
  
  ;; single vertex→vertex path.
  (is (april (with (:space graph-demo-space))
	     "g path 2 1 ")
      #(2 3 4 1) :test #'equalp)
  
  ;; multiple vertex→vertex path.
  (is (april (with (:space graph-demo-space))
	     "g path (1 2)(4 5)")
      #(2 3 4) :test #'equalp)

  ;; each to every single vertex.
  (is (april (with (:space graph-demo-space))
	     "g∘path¨⍳5 5")
      #2A((#(1) #(1 2) #(1 3) #(1 3 4) #(1 3 4 5))
	  (#(2 3 4 1) #(2) #(2 3) #(2 3 4) #(2 3 4 5))
	  (#(3 4 1) #(3 2) #(3) #(3 4) #(3 4 5))
	  (#(4 1) #(4 1 2) #(4 5 3) #(4) #(4 5))
	  (#(5 3 4 1) #(5 3 2) #(5 3) #(5 3 4) #(5)))
      :test #'equalp)

  (setq prove:*enable-colors* t))

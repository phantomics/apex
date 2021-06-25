;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; patterns.lisp

(in-package #:april)

"A set of optimization patterns for April; these patterns are matched before more basic language structures are recognized by the compiler. Optimized code for common APL language idioms is implemented in this way."

(composer-pattern sum-until-pattern (iota iota-props slash slash-props plus plus-props)
    ((assign-element iota iota-props process-function '(:glyph ⍳))
     (assign-element slash slash-props process-operator '(:glyph /))
     (assign-element plus plus-props process-function '(:glyph +)))
  (if (and iota slash plus)
      (let ((arg (gensym)) (var (gensym)))
	(values `(lambda (,arg) (loop :for ,var :from 0 :to (disclose ,arg) :summing ,var))
		'(:type (:function :implicit :sum-until-pattern))))
      (values nil nil tokens)))

(composer-pattern get-last-pattern (comma comma-props rotate rotate-props disclose disclose-props value)
    ((assign-element comma comma-props process-function '(:glyph \,))
     (assign-element rotate rotate-props process-function '(:glyph ⌽))
     (if (not rotate) (assign-element rotate rotate-props process-function '(:glyph ⊖)))
     (assign-element disclose disclose-props process-function '(:glyph ⊃))
     (assign-element value value-props process-value)) ;; doesn't work if a left arg is present
  (if (and comma rotate disclose (not value))
      (let ((input (gensym)))
	(values `(lambda (,input)
		   (if (not (arrayp ,input))
   		       ,input (row-major-aref ,input (1- (array-total-size ,input)))))
		'(:type (:function :implicit :get-last-pattern))))
      (values nil nil tokens)))

(composer-pattern rank-pattern (shape1 shape1-props shape2 shape2-props value)
    ((assign-element shape1 shape1-props process-function '(:glyph ⍴))
     (assign-element shape2 shape2-props process-function '(:glyph ⍴))
     (assign-element value value-props process-value)) ;; doesn't work if a left arg is present
  (if (and shape1 shape2 (not value))
      (let ((input (gensym)))
	(values `(lambda (,input) (vector (rank ,input)))
		'(:type (:function :implicit :rank-pattern))))
      (values nil nil tokens)))

(defvar *composer-optimized-opening-patterns-common*)

(setq *composer-optimized-opening-patterns-common*
      '((:name :sum-until-pattern :function sum-until-pattern)
	(:name :get-last-pattern :function get-last-pattern)
	(:name :rank-pattern :function rank-pattern)))

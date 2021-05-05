;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Aplesque -*-
;;;; package.lisp

(defpackage #:aplesque
  (:export #:varef #:array-promote #:array-match #:array-depth #:section #:array-to-list #:apply-marginal
	   #:expand #:enlist #:array-inner-product #:index-of #:grade #:array-grade #:nest #:enclose
	   #:alpha-compare #:array-compare #:find-array #:ravel #:across #:re-enclose #:duplicate
	   #:reshape-to-fit #:sprfact #:binomial #:scale-array #:mix-arrays #:array-inner-product
	   #:is-unitary #:choose #:catenate #:laminate #:enclose-atom #:partitioned-enclose #:split-array
	   #:invert-matrix #:interval-index #:turn #:partition-array #:stencil #:count-segments
	   #:array-impress #:matrix-print #:disclose-unitary #:apply-scalar #:reduce-array
	   #:get-first-or-disclose #:assign-element-type #:type-in-common #:initialize-for-environment
	   #:array-outer-product #:inverse-outer-product #:copy-nested-array #:disclose #:xdotimes
	   #:get-dimensional-factors)
  (:use #:cl #:cl-ppcre #:alexandria #:array-operations #:parse-number #:symbol-munger #:lparallel)
  (:shadowing-import-from #:array-operations #:flatten)
  (:shadowing-import-from #:cl-ppcre #:split))

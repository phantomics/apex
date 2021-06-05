;;;; S-Expression parser from “esrap/example-sexp.lisp” ported to MaxPC.

(defpackage maxpc.example-sexp
  (:use :cl :maxpc :maxpc.char :maxpc.digit)
  (:export :=sexp))

(in-package :maxpc.example-sexp)

;;; A semantic predicate for filtering out double quotes.

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

;;; Utility rules.

(defun ?alphanumeric ()
  (?satisfies 'alphanumericp))

(defun ?string-char ()
  (%or (?seq (?eq #\\) (?eq #\"))
       (?satisfies 'not-doublequote)))

;;; Here we go: an S-expression is either a list or an atom, with possibly
;;; leading whitespace.

(defun =atom ()
  (%or (=string) (=integer-number) (=symbol)))

(defun =string ()
  (=destructure (_ s _)
      (=list (?eq #\")
             (=subseq (%any (?string-char)))
             (?eq #\"))))

(defun =symbol ()
  ;; NOT-INTEGER is not strictly needed because ATOM considers INTEGER before a
  ;; STRING, we know can accept all sequences of alphanumerics -- we already
  ;; know it isn't an integer.
  (=transform (=subseq (?satisfies 'not-integer
                                   (=subseq (%some (?alphanumeric)))))
              'intern))

(defun =sexp ()
  (%or '=slist/parser (=atom)))

(defun =slist ()
  (=destructure (_ expressions _ _)
      (=list (?eq #\()
             (%any (=destructure (_ expression)
                       (=list (%any (?whitespace)) '=sexp/parser)))
             (%any (?whitespace))
             (?eq #\)))))

;; Recursive parsers hack.
(setf (fdefinition '=sexp/parser) (=sexp)
      (fdefinition '=slist/parser) (=slist))

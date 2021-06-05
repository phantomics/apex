;;;; Various microbenchmarks.

(defpackage maxpc.bench
  (:use :cl :maxpc)
  (:export :bench-=destructure
           :bench-=destructure/bare))

(in-package :maxpc.bench)

(defun bench-=destructure (iterations)
  (let ((input (make-array (truncate (* 2 iterations))
                           :element-type 'base-char
                           :initial-element #\x))
        (parser (%any (?seq (=destructure (_ _)
                                (=list (=element) (=element)))))))
    (time (parse input parser))))

(defun bench-=destructure/bare (iterations)
  (let* ((p (lambda (input) (values input t t)))
         (dp (=destructure (_ _) (=list p p)))
         (input (maxpc.input:make-input "")))
    (time (loop for i from 1 to (/ iterations 2) do
               (funcall dp input)))))

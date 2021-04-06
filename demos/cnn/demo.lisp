;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Cnn -*-
;;;; demo.lisp

(in-package #:april-demo.cnn)

(defparameter *package-symbol* (intern (package-name *package*) "KEYWORD"))

;; binary format for .idx files
(defbinary idx-file (:byte-order :big-endian)
  (empty 0 :type 16)
  (type 0 :type 8)
  (rank 0 :type 8)
  (dimensions #() :type (simple-array (unsigned-byte 32) (rank)))
  (data #() :type (eval (case type (#x08 `(simple-array (unsigned-byte 8) (,(april-c "{×/⍵}" dimensions))))
  			  (#x09 `(simple-array (signed-byte 8) (,(april-c "{×/⍵}" dimensions))))
  			  (#x0b `(simple-array (signed-byte 16) (,(april-c "{×/⍵}" dimensions))))
  			  (#x0c `(simple-array (signed-byte 32) (,(april-c "{×/⍵}" dimensions))))
  			  (#x0d `(simple-array single-float (,(april-c "{×/⍵}" dimensions))))
  			  (#x0e `(simple-array double-float (,(april-c "{×/⍵}" dimensions))))))))

(defun idx-file-to-array (file-path)
  "Load the contents of an .idx file into an array."
  (with-open-binary-file (in-raw file-path :direction :input)
    (with-wrapped-in-bit-stream (in in-raw :byte-order :big-endian)
      (let ((idx-input (read-binary 'idx-file in)))
	(if (= 1 (slot-value idx-input 'rank))
	    (slot-value idx-input 'data)
	    (make-array (loop :for d :across (slot-value idx-input 'dimensions) :collect d)
	 		:displaced-to (slot-value idx-input 'data)
	 		:element-type (array-element-type (slot-value idx-input 'data))))))))

(let ((training-data) (training-labels) (test-data) (test-labels))
  (defun load-idx-files ()
    "Load data from .idx files in input/ directory into four variables."
    (setq training-data (idx-file-to-array (asdf:system-relative-pathname *package-symbol*
									  "input/train-images.idx3-ubyte"))
	  training-labels (idx-file-to-array (asdf:system-relative-pathname *package-symbol*
									    "input/train-labels.idx1-ubyte"))
	  test-data (idx-file-to-array (asdf:system-relative-pathname *package-symbol*
								      "input/t10k-images.idx3-ubyte"))
	  test-labels (idx-file-to-array (asdf:system-relative-pathname *package-symbol*
									"input/t10k-labels.idx1-ubyte")))
    "Data loaded.")
  ;; these functions fetch the input data
  (defun get-training-data () training-data)
  (defun get-training-labels () training-labels)
  (defun get-test-data () test-data)
  (defun get-test-labels () test-labels))

(april-load (with (:space cnn-demo-space))
	    (asdf:system-relative-pathname (intern (package-name *package*) "KEYWORD") "cnn.apl"))

(defun train ()
  "Train a convolutional neural network with a set of training data and test it against another dataset."
  (april (with (:space cnn-demo-space)
	       (:state :in ((trimgs (get-training-data)) (trlabs (get-training-labels))
			    (teimgs (get-test-data)) (telabs (get-test-labels)))))
	 "
{
epochs    ← 10
batchSize ← 1
trainings ← 5 ⍝ 1000
tests     ← 10000
rate      ← 0.05
k1        ← 6 5 5⍴÷25
b1        ← 6⍴÷6
k2        ← 12 6 5 5⍴÷150
b2        ← 12⍴÷12
fc        ← 10 12 1 4 4⍴÷192
b         ← 10⍴÷10
index     ← 1
startTime ← timeFactors⊥¯4↑⎕ts

⎕ ← 'Running Zhang with ',(⍕epochs),' epochs, batch size ',(⍕batchSize),','
⎕ ← (⍕trainings),' training images, ',(⍕tests),' tests and a rate of ',(⍕rate),'.'
⎕ ← '  ' ⋄ ⎕ ← '--' ⋄ ⎕ ← '  '

(k1 b1 k2 b2 fc b) ← {
  t ← timeFactors⊥¯4↑⎕ts

  (e k1 b1 k2 b2 fc b) ← train (0 0), ⍵, rate trimgs trlabs trainings

  ⎕ ← 'Training epoch ',({⍵,⍨'0'⍴⍨(⍴⍕epochs)-⍴⍵}⍕index),' completed in ',formatElapsed t
  ⎕ ← 'Average error after training: ',(⍕e) ⋄ ⎕ ← '  '
  index+←1

  k1 b1 k2 b2 fc b
}⍣epochs⊢k1 b1 k2 b2 fc b

⎕ ← 'Training complete, now running tests...'

t       ← timeFactors⊥¯4↑⎕ts
correct ← +/telabs = teimgs testZhang⍤2⊢k1 b1 k2 b2 fc b

⎕ ← '  ' ⋄ ⎕ ← '--' ⋄ ⎕ ← '  '
⎕ ← 'Recognition testing completed in ',formatElapsed t
⎕ ← (⍕correct),' images out of ',(⍕tests),' recognized correctly'
⎕ ← '  ' ⋄ ⎕ ← 'Total time: ',formatElapsed startTime ⋄ ⎕ ← '  '

} 0")
  :tests-complete)

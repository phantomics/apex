;;;; april-demo.cnn.asd

(asdf:defsystem #:april-demo.cnn
  :description "Demo of April used to implement convolutional neural network"
  :author "Andrew Sengul"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ("april" "lisp-binary")
  :components ((:file "package")
	       (:file "setup")
               (:file "demo")))

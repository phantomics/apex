;;;; april-demo.dfns.array.asd

(asdf:defsystem #:april-demo.dfns.array
  :description "Demo of April used to implement Dyalog array functions"
  :author "Andrew Sengul"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ("april")
  :components ((:file "package")
	       (:file "setup")
               (:file "demo")))

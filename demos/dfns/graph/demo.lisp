;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Dfns.Graph -*-
;;;; demo.lisp

(in-package :april-demo.dfns.graph)

(april-load (with (:space graph-demo-space)) ;; load array.apl for foldl function
	    (asdf:system-relative-pathname :april-demo.dfns.array "array.apl"))

(april-load (with (:space graph-demo-space))
	    (asdf:system-relative-pathname (intern (package-name *package*) "KEYWORD") "graph.apl"))

(specify-demo
 "April graph demo"
 (with :space graph-demo-space
       :description "Implements graph processing functions from Dyalog's dfns.")
 (:tests (provision "g ← (2 3) (3) (2 4) (1 5) (3)")
	 (is "g gperm 2 1 3 4 5" #(3 #(1 3) #(1 4) #(2 5) 3))
	 (is "g gperm ⌽⍳⍴g" #(3 #(5 1) #(4 2) 3 #(4 3)))
	 (is "g insnode 10" #(#(2 3) 3 #(2 4) #(1 5) 3 #() #() #() #() #()))
	 (is "g remnode 3" #(#(2) #() #(1 4) #()))
	 (is "g remnode foldl 1" #(#(2) #(1 3) #(4) #(2)))
	 (is "g remnode foldl 1 1" #(#(2) #(3) #(1)))
	 (is "g remnode foldl 5 4" #(#(2 3) #(3) #(2)))
	 (is "g inslink 5 1" #(#(2 3) 3 #(2 4) #(1 5) #(3 1)))
	 (is "g remlink 2 3" #(#(2 3) #() #(2 4) #(1 5) 3))
	 (is "g search 3" #(3 2 4 1 5))
	 (is "g path 2 1 " #(2 3 4 1))
	 (is "g path (1 2)(4 5)" #(2 3 4))
	 (is "g∘path¨⍳5 5" #2A((#(1) #(1 2) #(1 3) #(1 3 4) #(1 3 4 5))
			       (#(2 3 4 1) #(2) #(2 3) #(2 3 4) #(2 3 4 5))
			       (#(3 4 1) #(3 2) #(3) #(3 4) #(3 4 5))
			       (#(4 1) #(4 1 2) #(4 5 3) #(4) #(4 5))
			       (#(5 3 4 1) #(5 3 2) #(5 3) #(5 3 4) #(5))))
	 (is "g span 1" #(-1 1 1 3 4))
	 (is "g span 3" #(4 3 -1 3 4))
	 (is "g∘span¨⍳⍴g" #(#(-1 1 1 3 4) #(4 -1 2 3 4) #(4 3 -1 3 4) #(4 1 5 -1 4) #(4 3 5 3 -1)))
	 (is "g dfspan 1" #(-1 1 2 3 4))
	 (is "g dfspan 3" #(4 3 -1 3 4))
	 (is "stdists¨g∘span¨⍳⍴g" #(#(0 1 1 2 3) #(3 0 1 2 3) #(2 1 0 1 2) #(1 2 2 0 1) #(3 2 1 2 0)))
	 (is "(g span 3)∘stpath¨⍳5" #(#(3 4 1) #(3 2) #(3) #(3 4) #(3 4 5)))
	 (is "(g∘span¨⍳⍴g)∘.stpath⍳⍴g" #2A((#(1) #(1 2) #(1 3) #(1 3 4) #(1 3 4 5))
					   (#(2 3 4 1) #(2) #(2 3) #(2 3 4) #(2 3 4 5))
					   (#(3 4 1) #(3 2) #(3) #(3 4) #(3 4 5))
					   (#(4 1) #(4 1 2) #(4 5 3) #(4) #(4 5))
					   (#(5 3 4 1) #(5 3 2) #(5 3) #(5 3 4) #(5))))
	 (is "stpaths¨g∘span¨⍳⍴g" #(#(#*1 #(1 2) #(1 3) #(1 3 4) #(1 3 4 5))
				    #(#(2 3 4 1) #(2) #(2 3) #(2 3 4) #(2 3 4 5))
				    #(#(3 4 1) #(3 2) #(3) #(3 4) #(3 4 5))
				    #(#(4 1) #(4 1 2) #(4 5 3) #(4) #(4 5))
				    #(#(5 3 4 1) #(5 3 2) #(5 3) #(5 3 4) #(5))))))

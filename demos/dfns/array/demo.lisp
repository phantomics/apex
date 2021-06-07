;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Dfns.Array -*-
;;;; demo.lisp

(in-package :april-demo.dfns.array)

;; (april (with (:space array-demo-space)
;; 	     (:store-fun (test-print (lambda (item) (print item)))))
;;        "")

(april-load (with (:space array-demo-space))
	    (asdf:system-relative-pathname (intern (package-name *package*) "KEYWORD") "array.apl"))

(specify-demo
 "April array demo"
 (with :space array-demo-space
       :description "Implements graph processing functions from Dyalog's dfns.")
 (:tests (provision "found ← ('milly' 'molly' 'may') ('star' 'thing' 'stone')")
	 (is "found alget 'may'" "stone")
	 (is "found alpop 'molly'" #("thing" #(#("milly" "may") #("star" "stone"))))
	 (is "found alset 'may' 'pebble'" #(#("milly" "molly" "may") #("star" "thing" "pebble")))
	 (is "found alpush 'may' 'rock'" #(#("may" "milly" "molly" "may") #("rock" "star" "thing" "stone")))
	 (provision "vecs←(('hello' 'world')('bonjour' 'monde'))(('good' 'night')('bon' 'soir'))")
	 (is "'abracadabra' {⍺~⍵⊃⍺} foldl 1 2" "bcdb")
	 (is "'abracadabra' {⍺~⍵⊃⍺} foldl 2 1" "rcdr")
	 (is "0 ,∘⊂⍨ foldl 2 5⍴⍳10" #2A((5 #(4 #(3 #(2 #*10)))) (10 #(9 #(8 #(7 #(6 0)))))))
	 ;; (is "g gperm ⌽⍳⍴g" #(3 #(5 1) #(4 2) 3 #(4 3)))
	 ;; (is "g insnode 10" #(#(2 3) 3 #(2 4) #(1 5) 3 #() #() #() #() #()))
	 ;; (is "g remnode 3" #(#(2) #() #(1 4) #()))
	 ;; ;; TODO: add foldl remnode cases once foldl is in
	 ;; (is "g inslink 5 1" #(#(2 3) 3 #(2 4) #(1 5) #(3 1)))
	 ;; (is "g remlink 2 3" #(#(2 3) #() #(2 4) #(1 5) 3))
	 ;; (is "g search 3" #(3 2 4 1 5))
	 ;; (is "g path 2 1 " #(2 3 4 1))
	 ;; (is "g path (1 2)(4 5)" #(2 3 4))
	 ;; (is "g∘path¨⍳5 5" #2A((#(1) #(1 2) #(1 3) #(1 3 4) #(1 3 4 5))
	 ;; 		       (#(2 3 4 1) #(2) #(2 3) #(2 3 4) #(2 3 4 5))
	 ;; 		       (#(3 4 1) #(3 2) #(3) #(3 4) #(3 4 5))
	 ;; 		       (#(4 1) #(4 1 2) #(4 5 3) #(4) #(4 5))
	 ;; 		       (#(5 3 4 1) #(5 3 2) #(5 3) #(5 3 4) #(5))))
	 ;; (is "g span 1" #(-1 1 1 3 4))
	 ;; (is "g span 3" #(4 3 -1 3 4))

	 ))

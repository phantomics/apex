;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Dfns.Array -*-
;;;; demo.lisp

(in-package :april-demo.dfns.array)

 (april (with (:space array-demo-space)
 	     (:store-fun (test-print (lambda (item) (print item)))))
        "")

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
	 (is "0 enlist vecs" #(#\h #\e #\l #\l #\o #\w #\o #\r #\l #\d #\b #\o #\n #\j #\o #\u #\r #\m #\o
			       #\n #\d #\e #\g #\o #\o #\d #\n #\i #\g #\h #\t #\b #\o #\n #\s #\o #\i #\r))
	 (is "1 enlist vecs" #("hello" "world" "bonjour" "monde" "good" "night" "bon" "soir"))
	 (is "2 enlist vecs" #(#("hello" "world") #("bonjour" "monde") #("good" "night") #("bon" "soir")))
	 (is "3 enlist vecs" #(#(#("hello" "world") #("bonjour" "monde"))
			       #(#("good" "night") #("bon" "soir"))))
	 (provision "tea ← 2 2⍴('tea'4'two')(2'for' 'T')('me' '&' 'you')('u' 'and' 'me')")
	 (is "0 enlist tea" #(#\t #\e #\a 4 #\t #\w #\o 2 #\f #\o #\r #\T #\m #\e #\& #\y #\o #\u #\u #\a
			      #\n #\d #\m #\e))
	 (is "1 enlist tea" #("tea" 4 "two" 2 "for" #\T "me" #\& "you" #\u "and" "me"))
	 (is "2 enlist tea" #(#("tea" 4 "two") #(2 "for" #\T) #("me" #\& "you") #(#\u "and" "me")))
	 (is "'abracadabra' {⍺~⍵⊃⍺} foldl 1 2" "bcdb")
	 (is "'abracadabra' {⍺~⍵⊃⍺} foldl 2 1" "rcdr")
	 (is "0,∘⊂⍨ foldl 2 5⍴⍳10" #2A((5 #(4 #(3 #(2 #*10)))) (10 #(9 #(8 #(7 #(6 0)))))))
	 (is "nlines 2 3 4⍴⍳24" 7)
	 (is "nlines 2 2 2 2⍴2" 12)
	 (is "nlines 2 0 4⍴2" 1)
	 (is "nlines 2 3 0 0⍴2" 6)
	 (is "nlines 1 2 3 4 5 6 7 8⍴9" 5907)
	 (is "1(2 3),perv(4 5)6" #(#(#(1 4) #(1 5)) #(#(2 6) #(3 6))))))

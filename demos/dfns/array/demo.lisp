;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Dfns.Array -*-
;;;; demo.lisp

(in-package :april-demo.dfns.array)

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
	 (is ",acc ⍳4" #(#(1 2 3 4) #(2 3 4) #(3 4) 4))
	 (is "+acc ⍳4" #(10 9 7 4))
	 (is ",acc 2/¨⍳4" #(#(1 1 2 2 3 3 4 4) #(2 2 3 3 4 4) #(3 3 4 4) #(4 4)))
	 ;; TODO: some acc tests still have bugs
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
	 (is "1(2 3),perv(4 5)6" #(#(#(1 4) #(1 5)) #(#(2 6) #(3 6))))
	 (is "2 3 3 2 +pred ⍳10" #(3 12 21 19))
	 (is "pmat 3" #2A((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
	 (is "{⍵[pmat⍴⍵]}'tic' 'tac' 'toe'" #2A(("tic" "tac" "toe") ("tic" "toe" "tac")	("tac" "tic" "toe")
						("tac" "toe" "tic") ("toe" "tic" "tac") ("toe" "tac" "tic")))
	 (is "4 3 2⍴↓{⍵[pmat⍴⍵]}'abcd'" #3A((("abcd" "abdc") ("acbd" "acdb") ("adbc" "adcb"))
					    (("bacd" "badc") ("bcad" "bcda") ("bdac" "bdca"))
					    (("cabd" "cadb") ("cbad" "cbda") ("cdab" "cdba"))
					    (("dabc" "dacb") ("dbac" "dbca") ("dcab" "dcba"))))
	 (is "{⍵⍳⍵∘.{⍺⊃¨⊂⍵}⍵}↓pmat 3" #2A((1 2 3 4 5 6) (2 1 4 3 6 5)
					  (3 5 1 6 2 4) (4 6 2 5 1 3)
					  (5 3 6 1 4 2) (6 4 5 2 3 1)))))

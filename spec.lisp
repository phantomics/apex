;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; spec.lisp

(in-package #:april)

"This specification defines the April language. All of the standard functions and operators and their symbols, along with the language's grammar, utilities, reserved symbols, tests and demo suite are specified here."

(defvar *digit-vector* "0123456789")

(defvar *alphabet-vector* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defvar *idiom-native-symbols* '(⍺ ⍵ ⍺⍺ ⍵⍵ ∇ ∇∇ index-origin print-precision *digit-vector* *alphabet-vector*
				 *apl-timestamp* to-output output-stream))

(let ((circular-functions ;; APL's set of circular functions called using the ○ symbol with a left argument
       (vector (lambda (x) (exp (complex 0 x)))
	       (lambda (x) (complex 0 x))
	       #'conjugate #'identity (lambda (x) (- (sqrt (- (1+ (expt x 2))))))
	       #'atanh #'acosh #'asinh (lambda (x) (if (= -1 x) 0 (* (1+ x) (sqrt (/ (1- x) (1+ x))))))
	       #'atan #'acos #'asin (lambda (x) (sqrt (- 1 (expt x 2))))
	       #'sin #'cos #'tan (lambda (x) (sqrt (1+ (expt x 2))))
	       #'sinh #'cosh #'tanh (lambda (x) (sqrt (- (1+ (expt x 2)))))
	       #'realpart #'abs #'imagpart #'phase)))
  (defun call-circular (&optional inverse)
    (lambda (value function-index)
      (if (and (integerp function-index) (<= -12 function-index 12))
	  (funcall (aref circular-functions (+ 12 (funcall (if inverse #'- #'identity)
							   function-index)))
		   (* 1.0d0 value))
	  (error "Invalid argument to [○ circular]; the left argument must be an~a"
		 " integer between ¯12 and 12.")))))

;; top-level specification for the April language
(specify-vex-idiom
 april

 ;; system variables and default state of an April workspace
 (system :output-printed nil
	 :base-state '(:output-stream '*standard-output*)
	 :workspace-defaults '(:index-origin 1 :print-precision 10 :comparison-tolerance double-float-epsilon))

 ;; standard grammar components, with elements to match the basic language forms and
 ;; pattern-matching systems to register combinations of those forms
 (grammar (:opening-patterns *composer-opening-patterns*)
	  (:following-patterns *composer-following-patterns*
			       ;; composer-optimized-patterns-common
			       ))

 ;; parameters for describing and documenting the idiom in different ways; currently, these options give
 ;; the order in which output from the blocks of tests is printed out for the (test) and (demo) options
 (doc-profiles (:test :lexical-functions-scalar-numeric :lexical-functions-scalar-logical
		      :lexical-functions-array :lexical-functions-special :lexical-operators-lateral
		      :lexical-operators-pivotal :lexical-operators-unitary :general-tests
		      :system-variable-function-tests :function-inversion-tests
		      :printed-format-tests)
	       (:arbitrary-test :output-specification-tests)
	       (:time :lexical-functions-scalar-numeric :lexical-functions-scalar-logical
	       	      :lexical-functions-array :lexical-functions-special :lexical-operators-lateral
	       	      :lexical-operators-pivotal :lexical-operators-unitary :general-tests)
	       (:demo :general-tests :lexical-functions-scalar-numeric :lexical-functions-scalar-logical
		      :lexical-functions-array :lexical-functions-special :lexical-operators-lateral
		      :lexical-operators-pivotal :lexical-operators-unitary :system-variable-function-tests
		      :function-inversion-tests :printed-format-tests))

 ;; utilities for compiling the language
 (utilities :match-blank-character (lambda (char) (member char '(#\  #\Tab) :test #'char=))
	    :match-newline-character (lambda (char) (member char '(#\⋄ #\◊ #\Newline #\Return) :test #'char=))
	    :match-inline-newline-character (lambda (char) (member char '(#\⋄ #\◊) :test #'char=))
	    ;; set the language's valid blank, newline characters and token characters
	    :match-token-character
	    (lambda (char)
	      (or (alphanumericp char)
		  (not (loop :for c :across "._⎕∆⍙∇¯⍺⍵⍬" :never (char= c char)))))
	    ;; overloaded numeric characters may be functions or operators or may be part of a numeric token
	    ;; depending on their context
	    :match-overloaded-numeric-character (lambda (char) (char= #\. char))
	    ;; this code preprocessor removes comments: everything between a ⍝ and newline character
	    :prep-code-string
	    (lambda (string)
	      (let ((commented) (osindex 0)
		    (out-string (make-string (length string) :initial-element #\ )))
		(loop :for char :across string
		   :do (if commented (if (member char '(#\Newline #\Return) :test #'char=)
					 (setf commented nil
					       (row-major-aref out-string osindex) char
					       osindex (1+ osindex)))
			   (if (char= char #\⍝)
			       (setq commented t)
			       (setf (row-major-aref out-string osindex) char
				     osindex (1+ osindex)))))
		out-string))
	    ;; handles axis strings like "'2;3;;' from 'array[2;3;;]'"
	    :process-axis-string
	    (lambda (string)
	      (let ((indices) (last-index)
		    (nesting (vector 0 0 0))
		    (delimiters '(#\[ #\( #\{ #\] #\) #\})))
		(loop :for char :across string :counting char :into charix
		   :do (let ((mx (length (member char delimiters))))
			 (if (< 3 mx) (incf (aref nesting (- 6 mx)))
			     (if (< 0 mx 4) (if (< 0 (aref nesting (- 3 mx)))
						(decf (aref nesting (- 3 mx)))
						(error "Each closing ~a must match with an opening ~a."
						       (nth mx delimiters) (nth (- 3 mx) delimiters)))
				 (if (and (char= char #\;)
					  (= 0 (loop :for ncount :across nesting :summing ncount)))
				     (setq indices (cons (1- charix) indices)))))))
		(loop :for index :in (reverse (cons (length string) indices))
		   :counting index :into iix
		   :collect (make-array (- index (if last-index 1 0)
					   (if last-index last-index 0))
					:element-type 'character :displaced-to string
					:displaced-index-offset (if last-index (1+ last-index) 0))
		   :do (setq last-index index))))
	    ;; macro to process lexical specs of functions and operators
	    :process-lexicon #'april-function-glyph-processor
	    :test-parameters '((:space unit-test-staging))
	    :format-value #'format-value
	    ;; process system state input passed as with (april (with (:state ...)) "...")
	    :preprocess-state-input
	    (lambda (state)
	      (if (getf state :count-from) (setf (getf state :index-origin)
						 (getf state :count-from)))
	      state)
	    ;; converts parts of the system state into lists that will form part of the local lexical
	    ;; environment in which the compiled APL code runs, i.e. the (let) form into which
	    ;; the APL-generating macros are expanded
	    :system-lexical-environment-interface
	    (lambda (state)
	      ;; the index origin, print precision and output stream values are
	      ;; passed into the local lexical environment
	      (append (list (list (intern "OUTPUT-STREAM" *package-name-string*)
				  (if (getf state :print-to)
				      (getf state :print-to)
				      (second (getf state :output-stream)))))
		      (list (list 'index-origin (or (getf state :index-origin)
						    `(inws *index-origin*)))
			    (list 'print-precision (or (getf state :print-precision)
						       `(inws *print-precision*)))
			    (list 'comparison-tolerance (or (getf state :comparison-tolerance)
							    `(inws *comparison-tolerance*))))))
	    :lexer-postprocess
	    (lambda (tokens idiom space)
	      ;; currently, this function is used to initialize function and variable references
	      ;; in the workspace before compilation is performed so that recursive
	      ;; functions will work correctly as with fn←{A←⍵-1 ⋄ $[A≥0;A,fn A;0]} ⋄ fn 5
	      (match tokens
	    	((list (guard fn-form (and (listp fn-form)
	    				   (eq :fn (first fn-form))
	    				   (listp (second fn-form))))
	    	       '(:fn #\←) (guard symbol (and (symbolp symbol)
	    					     (not (member symbol '(⍺⍺ ⍵⍵))))))
	    	 (if (is-workspace-value symbol)
	    	      (makunbound (intern (string symbol) space)))
	    	 (if (not (fboundp (intern (string symbol) space)))
	    	     (setf (symbol-function (intern (string symbol) space)) #'dummy-nargument-function)))
		((list (guard op-form (and (listp op-form)
	    				   (eq :op (first op-form))
	    				   (listp (second op-form))))
	    	       '(:fn #\←) (guard symbol (and (symbolp symbol)
	    					     (not (member symbol '(⍺⍺ ⍵⍵))))))
		 (let ((fn-symbol (intern (string symbol) space))
		       (symbol (intern (concatenate 'string
						    (if (eq :lateral (getf (second op-form) :valence))
							"𝕆𝕃∇" "𝕆ℙ∇")
						    (string symbol))
				       space)))
		   ;; if the symbol is already bound as a regular function, unbind it
		   (if (fboundp fn-symbol) (fmakunbound fn-symbol))
	    	   (if (not (fboundp symbol))
	    	       (setf (symbol-function symbol) #'dummy-nargument-function)))))
	      (labels ((processor (&optional valence)
			 (lambda (token list index)
			   (if (eql '∇∇ token)
			       (setf (nth index list) (list :op valence '∇∇))
			       (if (and (listp token) (listp (second token))
					(third token))
				   (assign-self-refs-among-tokens
				    (third token)
				    (processor (getf (second token) :valence))))))))
		(assign-self-refs-among-tokens tokens (processor)))
	      tokens)
	    :postprocess-compiled
	    (lambda (state &rest inline-arguments)
	      (lambda (form)
		(let ((final-form (if inline-arguments `(apl-call :fn ,(first (last form)) ,@inline-arguments)
				      (first (last form)))))
		  (append (butlast form)
			  (list (append (list 'apl-output final-form)
					(append (list :print-precision 'print-precision)
						(if (getf state :print) (list :print-to 'output-stream))
						(if (getf state :output-printed)
						    (list :output-printed (getf state :output-printed))))))))))
	    :postprocess-value
	    (lambda (form state)
	      (append (list 'apl-output form)
		      (append (list :print-precision 'print-precision)
			      (if (getf state :print) (list :print-to 'output-stream))
			      (if (getf state :output-printed)
				  (list :output-printed (getf state :output-printed))))))
	    :process-stored-symbol
	    (lambda (symbol space is-function)
	      (if is-function (progn (if (and (boundp (intern symbol space))
					      (not (fboundp (intern symbol space))))
					 (makunbound (intern symbol space)))
				     (setf (symbol-function (intern symbol space))
					   #'dummy-nargument-function))
		  (progn (if (fboundp (intern symbol space))
			     (fmakunbound (intern symbol space)))
			 (if (not (boundp (intern symbol space)))
			     (progn (proclaim (list 'special (intern symbol space)))
				    (set (intern symbol space) nil))))))
	    :build-variable-declarations #'build-variable-declarations
	    :build-compiled-code #'build-compiled-code)

 ;; specs for multi-character symbols exposed within the language
 (symbols (:variable ⎕ to-output ⎕io *index-origin* ⎕pp print-precision
		       ⎕ost output-stream ⎕ct *comparison-tolerance*)
	  (:constant ⎕a *alphabet-vector* ⎕d *digit-vector* ⎕ts *apl-timestamp*)
	  (:function ⎕t coerce-type))
 
 ;; APL's set of functions represented by characters
 (functions
  (with (:name :lexical-functions-scalar-numeric)
	(:tests-profile :title "Scalar Numeric Function Tests")
	(:demo-profile :title "Scalar Numeric Function Demos"
		       :description "Scalar numeric functions change individual numeric values. They include basic arithmetic and other numeric operations, and they can be applied over arrays."))
  (+ (has :titles ("Conjugate" "Add"))
     (ambivalent :asymmetric-scalar conjugate +)
     (inverse (ambivalent conjugate :plain - :right-composed (reverse-op -) :commuted (λω (/ omega 2))))
     (tests (is "+5" 5)
	    (is "+5J2" #C(5 -2))
	    (is "1+1" 2)
	    (is "1+1 2 3" #(2 3 4))))
  (- (has :titles ("Negate" "Subtract"))
     (ambivalent :symmetric-scalar (reverse-op -))
     (inverse (ambivalent (reverse-op -) :plain (reverse-op -) :right-composed +))
     (tests (is "2-1" 1)
	    (is "7-2 3 4" #(5 4 3))))
  (× (has :titles ("Sign" "Multiply"))
     (ambivalent :asymmetric-scalar signum *)
     (inverse (dyadic :plain / :right-composed (reverse-op /) :commuted sqrt))
     (tests (is "×20 5 0 ¯7 3 ¯9" #(1 1 0 -1 1 -1))
	    (is "2×3" 6)
	    (is "4 5×8 9" #(32 45))))
  (÷ (has :titles ("Reciprocal" "Divide"))
     (ambivalent :symmetric-scalar (reverse-op /))
     (inverse (ambivalent (reverse-op /) :plain (reverse-op /) :right-composed *))
     (tests (is "6÷2" 3)
	    (is "12÷6 3 2" #(2 4 6))
	    (is "÷2 4 8" #(1/2 1/4 1/8))))
  (⋆ (has :titles ("Exponential" "Power") :aliases (*))
     (ambivalent :asymmetric-scalar exp (reverse-op :dyadic expt))
     (inverse (ambivalent log :plain log :right-composed (λωα (expt alpha (/ omega)))))
     (tests (is "⌊1000×⋆2" 7389)
	    (is "2⋆4" 16)
	    (is "⌊16⋆÷2" 4)))
  (⍟ (has :titles ("Natural Logarithm" "Logarithm"))
     (ambivalent :symmetric-scalar log)
     (inverse (ambivalent exp :plain (reverse-op expt) :right-composed (λωα (expt omega (/ alpha)))))
     (tests (is "⌊1000×⍟5" 1609)
	    (is "⌊2⍟8" 3)))
  (\| (has :titles ("Magnitude" "Residue"))
      (ambivalent :asymmetric-scalar abs mod)
      (tests (is "|55" 55)
	     (is "|¯33" 33)
	     (is "8|39" 7)))
  (! (has :titles ("Factorial" "Binomial"))
     (ambivalent :asymmetric-scalar sprfact binomial)
     (tests (is "!5" 120)
	    (is "5!12" 792)
            (is "{⍵∘.!⍵}¯3+⍳7" #2A((1 -1 0 0 0 0 0) (0 1 0 0 0 0 0)
				   (1 1 1 1 1 1 1) (-2 -1 0 1 2 3 4)
				   (3 1 0 0 1 3 6) (-4 -1 0 0 0 1 4)
				   (5 1 0 0 0 0 1)))))
  (⌈ (has :titles ("Ceiling" "Maximum"))
     (ambivalent :asymmetric-scalar ceiling (reverse-op max))
     (inverse (dyadic :commuted identity))
     (tests (is "⌈1.0001" 2)
	    (is "⌈1.9998" 2)
	    (is "3⌈0 1 2 3 4 5" #(3 3 3 3 4 5))))
  (⌊ (has :titles ("Floor" "Minimum"))
     (ambivalent :asymmetric-scalar floor (reverse-op min))
     (inverse (dyadic :commuted identity))
     (tests (is "⌊1.0001" 1)
	    (is "⌊1.9998" 1)
	    (is "3⌊0 1 2 3 4 5" #(0 1 2 3 3 3))))
  (? (has :titles ("Random" "Deal"))
     (ambivalent (scalar-function (λω (if (integerp omega)
					  (if (= 0 omega) (random 1.0d0)
					      (+ index-origin (random omega)))
					  (if (floatp omega)
					      (random omega)
					      (error "The right arguments to ? must be non-negative integers.")))))
		 (deal index-origin))
     (tests (is "⍴5?⍴⍳5" #(5))))
  (○ (has :titles ("Pi Times" "Circular"))
     (ambivalent :asymmetric-scalar (λω (* pi omega)) (call-circular))
     (inverse (ambivalent (λω (/ omega pi)) :plain (call-circular :inverse)
      			  :right-composed (λωα (declare (ignore omega alpha))
      					       (error "Inverse [○ circular] may not take an ~a"
						      "implicit right argument."))))
     (tests (is "⌊100000×○1" 314159)
	    (is "(⌊1000×1÷2⋆÷2)=⌊1000×1○○÷4" 1)
	    (is "⌊1000×1○⍳9" #(841 909 141 -757 -959 -280 656 989 412))
	    (is "⌈1 2 3○○.5 2 .25" #(1 1 1))
	    ;; omit asin and atanh from the tests below because they
	    ;; are not consistent across CL implementations
	    (is "⌊1000×⊃,/9 11○⊂(¯1 ¯7~⍨¯8+⍳16) ∘.○ 0 ¯2 2 ¯2J2 2J3.5"
		#2A((0 1316 1316 1734 2095 1570 3141 0 2325 1064)
		    (0 -1444 1443 -1735 2079 0 0 0 754 1038)
		    (0 -1733 1732 -1880 1940 1000 0 0 2128 3607)
		    (0 -1108 1107 -1312 1442 0 0 0 238 215)
		    (1570 3141 0 2325 1064 0 -1317 1316 -1735 -2096)
		    (1000 0 0 2128 3607 0 1732 1732 1879 -1941)
		    (0 -910 909 -3421 15069 0 0 0 -1510 -6885)
		    (1000 -417 -417 -1566 -6897 0 0 0 3297 -15043)
		    (0 2185 -2186 28 -2 0 0 0 1023 1001)
		    (1000 2236 2236 2128 2063 0 0 0 -1880 3392)
		    (0 -3627 3626 1509 -3397 0 0 0 3420 -1320)
		    (1000 3762 3762 -1566 -3524 0 0 0 -3298 -1273)
		    (0 -965 964 -1024 972 0 0 0 -29 23)
		    (0 0 0 1879 3392 1000 2236 2236 2128 -2064)))))
  (\~ (has :titles ("Not" "Without"))
      (ambivalent (scalar-function (λω (cond ((= 0 omega) 1)
					     ((= 1 omega) 0)
					     (t (error "Domain error: arguments to ~~ must be 1 or 0.")))))
		  #'without)
      (inverse (monadic (scalar-function
			 (λω (cond ((= 0 omega) 1)
				   ((= 1 omega) 0)
				   (t (error "Domain error: arguments to ~~ must be 1 or 0.")))))))
      (tests (is "~1 0 1" #(0 1 0))
	     (is "1 2 3 4 5 6 7~3 5" #(1 2 4 6 7))
	     (is "1 2 3 4~2" #(1 3 4))
	     (is "(⍳9)~2 2⍴⍳9" #(5 6 7 8 9))
	     (is "'MACARONI'~'ALFREDO'" "MCNI"))))

 (functions
  (with (:name :lexical-functions-scalar-logical)
	(:tests-profile :title "Scalar Logical Function Tests")
	(:demo-profile :title "Scalar Logical Function Demos"
		       :description "Scalar logical functions compare individual values, and like scalar numeric functions they can be applied over arrays."))
  (< (has :title "Less")
     (dyadic (scalar-function (boolean-op (compare-by '< comparison-tolerance))))
     (tests (is "3<1 2 3 4 5" #*00011)))
  (≤ (has :title "Less or Equal")
     (dyadic (scalar-function (boolean-op (compare-by '<= comparison-tolerance))))
     (tests (is "3≤1 2 3 4 5" #*00111)))
  (= (has :title "Equal")
     (dyadic (scalar-function (boolean-op (scalar-compare comparison-tolerance))))
     (tests (is "3=1 2 3 4 5" #*00100)
	    (is "'cat'='hat'" #*011)))
  (≥ (has :title "Greater or Equal")
     (dyadic (scalar-function (boolean-op (compare-by '>= comparison-tolerance))))
     (tests (is "3≥1 2 3 4 5" #*11100)))
  (> (has :title "Greater")
     (dyadic (scalar-function (boolean-op (compare-by '> comparison-tolerance))))
     (tests (is "3>1 2 3 4 5" #*11000)))
  (≠ (has :titles ("Unique Mask" "Not Equal"))
     (ambivalent #'unique-mask
		 (scalar-function (boolean-op (λωα (not (funcall (scalar-compare comparison-tolerance)
								 omega alpha))))))
     (tests (is "≠2 4 7 4 6 8 3 5 2 4 2 5 6 7" #*11101111000000)
	    (is "≠'ONE' 'TWO' 'ONE' 'THREE' 'TWO' 'THREE'" #*110100)
	    (IS "≠↑'ONE' 'TWO' 'ONE' 'THREE' 'TWO' 'THREE'" #*110100)
	    (is "3≠1 2 3 4 5" #*11011)
	    (is "'Harrison'≠'Bergeron'" #*11011100)))
  (∧ (has :title "And" :aliases (^))
     (dyadic (scalar-function (reverse-op lcm)))
     (tests (is "0 1 0 1∧0 0 1 1" #*0001)))
  (⍲ (has :title "Nand")
     (dyadic (scalar-function (boolean-op (λωα (not (= omega alpha 1))))))
     (tests (is "0 1 0 1⍲0 0 1 1" #*1110)))
  (∨ (has :title "Or")
     (dyadic (scalar-function (reverse-op gcd)))
     (tests (is "0 1 0 1∨0 0 1 1" #*0111)))
  (⍱ (has :title "Nor")
     (dyadic (scalar-function (boolean-op (λωα (= omega alpha 0)))))
     (tests (is "0 1 0 1⍱0 0 1 1" #*1000))))

 (functions
  (with (:name :lexical-functions-array)
	(:tests-profile :title "Array Function Tests")
	(:demo-profile :title "Array Function Demos"
		       :description "These functions affect entire arrays, changing their structure or deriving data from them in some way."))
  (⍳ (has :titles ("Interval" "Index Of"))
     (ambivalent (λω (count-to omega index-origin))
		 (λωα (index-of omega alpha index-origin)))
     (inverse (monadic (λω (inverse-count-to omega index-origin))))
     (tests (is "⍳5" #(1 2 3 4 5))
	    (is "⍳0" #())
	    (is "⍳⍴⍳5" #(1 2 3 4 5))
	    (is "⍳2 3" #2A((#*11 #(1 2) #(1 3)) (#(2 1) #(2 2) #(2 3))))
	    (is "⍳4 3" #2A((#*11 #(1 2) #(1 3)) (#(2 1) #(2 2) #(2 3))
			   (#(3 1) #(3 2) #(3 3)) (#(4 1) #(4 2) #(4 3))))
	    (is "⍳2 4 3" #3A(((#*111 #(1 1 2) #(1 1 3)) (#(1 2 1) #(1 2 2) #(1 2 3))
			      (#(1 3 1) #(1 3 2) #(1 3 3)) (#(1 4 1) #(1 4 2) #(1 4 3)))
			     ((#(2 1 1) #(2 1 2) #(2 1 3)) (#(2 2 1) #(2 2 2) #(2 2 3))
			      (#(2 3 1) #(2 3 2) #(2 3 3)) (#(2 4 1) #(2 4 2) #(2 4 3)))))
	    (is "((,2)⍳3),2 3⍳4" #(2 3))
	    (is "(,3)⍳⍳4" #(2 2 1 2))
	    (is "2 4⍳⍳5" #(3 1 3 2 3))
	    (is "'aabc'⍳'b'" 3)
	    (is "'THIS' 'IS' 'A' 'TEST'⍳'IS' 'IT'" #(2 5))
	    (is "'RAT' 'CAT' 'DOG'⍳⊂'DOG'" 3)
	    (is "(3 3⍴'CATRATDOG')⍳'RAT'" 2)
	    (is "(3 3⍴'CATRATDOG')⍳4 3⍴'RATDOGPIG'" #(2 3 4 2))))
  (⍴ (has :titles ("Shape" "Reshape"))
     (ambivalent #'shape (reshape-array (quote (inws *value-meta*))))
     (tests (is "⍴1" #())
	    (is "⍴1 2 3" #(3))
	    (is "⍴3 5⍴1" #(3 5))
	    (is "⍴⍴3 4⍴2" #(2))
	    (is "⍴⍴⍴4 5 6 7⍴3" #(1))
	    (is "⍴⍬" #(0))
	    (is "3⍴2" #(2 2 2))
	    (is "3⍴3" #(3 3 3))
	    (is "4 5⍴⍳3" #2A((1 2 3 1 2) (3 1 2 3 1) (2 3 1 2 3) (1 2 3 1 2)))
	    (is "⍬⍴5 6 7" 5)
	    (is "3⍴0⍴⊂2 2⍴5" #(#2A((0 0) (0 0)) #2A((0 0) (0 0)) #2A((0 0) (0 0))))))
  (⌷ (has :title "Index")
     (dyadic (λωαχ (at-index omega alpha axes index-origin)))
     (tests (is "1⌷3" 3)
	    (is "3⌷2 4 6 8 10" 6)
	    (is "3⌷⍳9" 3)
  	    (is "2 2⌷4 5⍴⍳9" 7)
  	    (is "2 3 4⌷4 5 6⍴⍳9" 1)
  	    (is "1 3⌷2 3 4⍴⍳5" #(4 5 1 2))
  	    (is "1 3⌷[1 3]2 3 4⍴⍳5" #(3 2 1))
	    (is "1⌷[2]3 3⍴⍳9" #(1 4 7))
  	    (is "(⊂4 5 2 6 3 7 1)⌷'MARANGA'" "ANAGRAM")
	    (is "(⍬,5) 1⌷5 5⍴⍳25" 21)
	    (is "(5 4) 1⌷5 5⍴⍳25" #(21 16))))
  (≡ (has :titles ("Depth" "Match"))
     (ambivalent #'find-depth (boolean-op array-compare))
     (tests (is "≡1" 0)
  	    (is "≡⍳3" 1)
  	    (is "≡(1 2)(3 4)" 2)
  	    (is "≡1 (2 3) (4 5 (6 7)) 8" -3)
	    (IS "≡↓↓2 3⍴⍳6" 3)
	    (IS "≡↓↓↓2 3⍴⍳6" 4)
	    (is "3≡3" 1)
	    (is "4≡2" 0)
	    (is "''≡''" 1)))
  (≢ (has :titles ("First Dimension" "Not Match"))
     (ambivalent #'find-first-dimension (boolean-op (lambda (omega alpha) (not (array-compare omega alpha)))))
     (tests (is "≢2" 1)
	    (is "≢1 2 3" 3)
   	    (is "≢2 3 4⍴⍳9" 2)
	    (is "5≢5" 0)
	    (is "3≢1" 1)))
  (∊ (has :titles ("Enlist" "Membership"))
     (ambivalent #'enlist #'membership)
     (tests (is "∊2" 2)
	    (is "∊2 2 2⍴⍳9" #(1 2 3 4 5 6 7 8))
	    (is "2 3∊2" #*10)
	    (is "3∊3 4 5" 1)
  	    (is "2 5 7∊1 2 3 4 5" #*110)
	    (is "'IS' 'IT' ∊ 'THIS' 'IS' 'A' 'TEST'" #*10)))
  (⍷ (has :title "Find")
     (dyadic #'find-array)
     (tests (is "5⍷5" 1)
	    (is "2⍷3 4⍴⍳9" #2A((0 1 0 0) (0 0 0 0) (0 0 1 0)))
	    (is "(2 2⍴6 7 1 2)⍷2 3 4⍴⍳9" #3A(((0 0 0 0) (0 1 0 0) (0 0 0 0))
  					     ((0 0 1 0) (0 0 0 0) (0 0 0 0))))))
  (⍸ (has :titles ("Where" "Interval Index"))
     (ambivalent (λω (where-equal-to-one omega index-origin))
  		 #'interval-index)
     (tests (is "⍸1" #(#()))
	    (is "⍸0" #())
	    (is "⍸0 0 1 0 1 0 0 1 1 0" #(3 5 8 9))
  	    (is "⍸3=2 3 4⍴⍳9" #(#(1 1 3) #(1 3 4) #(2 3 1)))
  	    (is "⍸(2 3 4⍴⍳9)∊3 5" #(#(1 1 3) #(1 2 1) #(1 3 4) #(2 1 2) #(2 3 1) #(2 3 3)))
	    (is "2 4 6 8⍸3" 1)
  	    (is "10 20 30 40⍸5 12 19 24 35 42 51" #(0 1 1 2 3 4 4))
  	    (is "(2 5⍴'RADIUS')⍸3 4 5⍴'BOXCAR'" #2A((0 1 0 0) (2 0 0 1) (0 0 2 0)))
  	    (is "(2 3 5⍴'ABCDEFHIJKLM')⍸3 3 5⍴'BOREAL'" #(1 2 1))))
  (\, (has :titles ("Ravel" "Catenate or Laminate"))
      (ambivalent (ravel-array index-origin)
		  (catenate-arrays index-origin))
      (tests (is ",5" #(5))
	     (is ",3 4⍴⍳9" #(1 2 3 4 5 6 7 8 9 1 2 3))
	     (is ",[1]3 3⍴⍳9" #2A((1 2 3) (4 5 6) (7 8 9)))
	     (is ",[⍬,1]3 3⍴⍳9" #2A((1 2 3) (4 5 6) (7 8 9)))
  	     (is ",[0.5]3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3))))
  	     (is ",[1.5]3 4⍴⍳9" #3A(((1 2 3 4)) ((5 6 7 8)) ((9 1 2 3))))
  	     (is ",[2.5]3 4⍴⍳9" #3A(((1) (2) (3) (4)) ((5) (6) (7) (8)) ((9) (1) (2) (3))))
  	     (is ",[1 2]2 3 3⍴⍳12" #2A((1 2 3) (4 5 6) (7 8 9) (10 11 12) (1 2 3) (4 5 6)))
  	     (is ",[2 3]2 3 3⍴⍳12" #2A((1 2 3 4 5 6 7 8 9) (10 11 12 1 2 3 4 5 6)))
  	     (is ",[1 2 3]2 3 3⍴⍳12" #(1 2 3 4 5 6 7 8 9 10 11 12 1 2 3 4 5 6))
	     (is "⊃,[1]/(⊂3 3)⍴¨⍳5" #2A((1 1 1) (1 1 1) (1 1 1) (2 2 2) (2 2 2) (2 2 2)
					(3 3 3) (3 3 3) (3 3 3) (4 4 4) (4 4 4) (4 4 4)
					(5 5 5) (5 5 5) (5 5 5)))
	     (is "⊃,[2]/(⊂3 3)⍴¨⍳5" #2A((1 1 1 2 2 2 3 3 3 4 4 4 5 5 5)
					(1 1 1 2 2 2 3 3 3 4 4 4 5 5 5)
					(1 1 1 2 2 2 3 3 3 4 4 4 5 5 5)))
	     (is "5 6,3" #(5 6 3))
	     (is "2,⍳3" #(2 1 2 3))
  	     (is "0,3 4⍴⍳9" #2A((0 1 2 3 4) (0 5 6 7 8) (0 9 1 2 3)))
	     (is "⍬,⍳5" #(1 2 3 4 5))
	     (is "⍬,3" #(3))
	     (is "(2 2⍴'a'),'*'" #2A((#\a #\a #\*) (#\a #\a #\*)))
  	     (is "0,[1]3 4⍴⍳9" #2A((0 0 0 0) (1 2 3 4) (5 6 7 8) (9 1 2 3)))
  	     (is "(3 6⍴⍳6),3 4⍴⍳9" #2A((1 2 3 4 5 6 1 2 3 4) (1 2 3 4 5 6 5 6 7 8)
  				       (1 2 3 4 5 6 9 1 2 3)))
  	     (is "(5 4⍴⍳6),[1]3 4⍴⍳9" #2A((1 2 3 4) (5 6 1 2) (3 4 5 6) (1 2 3 4)
  					  (5 6 1 2) (1 2 3 4) (5 6 7 8) (9 1 2 3)))
  	     (is "(6 7 8 9 0),⍪1 2 3 4 5" #2A((6 1) (7 2) (8 3) (9 4) (0 5)))
  	     (is "(2 3 4⍴⍳5),2 3⍴9" #3A(((1 2 3 4 9) (5 1 2 3 9) (4 5 1 2 9))
  					((3 4 5 1 9) (2 3 4 5 9) (1 2 3 4 9))))
  	     (is "(4 4⍴5),4 4 4⍴3" #3A(((5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3))
  				       ((5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3))
  				       ((5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3))
  				       ((5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3))))
  	     (is "1 2 3,4 5 6" #(1 2 3 4 5 6))
  	     (is "1 2 3,[1]4 5 6" #(1 2 3 4 5 6))
  	     (is "(3 4⍴5),[1]2 3 4⍴9" #3A(((5 5 5 5) (5 5 5 5) (5 5 5 5))
  					  ((9 9 9 9) (9 9 9 9) (9 9 9 9))
  					  ((9 9 9 9) (9 9 9 9) (9 9 9 9))))
  	     (is "(2 4⍴5),[2]2 3 4⍴9" #3A(((5 5 5 5) (9 9 9 9) (9 9 9 9) (9 9 9 9))
  					  ((5 5 5 5) (9 9 9 9) (9 9 9 9) (9 9 9 9))))
  	     (is "(2 3⍴5),[3]2 3 4⍴9" #3A(((5 9 9 9 9) (5 9 9 9 9) (5 9 9 9 9))
  					  ((5 9 9 9 9) (5 9 9 9 9) (5 9 9 9 9))))
  	     (is "1 2 3 4,[0.5]1 2 3 4" #2A((1 2 3 4) (1 2 3 4)))
  	     (is "1 2 3 4,[1.5]1 2 3 4" #2A((1 1) (2 2) (3 3) (4 4)))
  	     (is "(2 3⍴⍳9),[0.5]2 3⍴⍳9" #3A(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))
  	     (is "(2 3⍴⍳9),[2.5]2 3⍴⍳9" #3A(((1 1) (2 2) (3 3)) ((4 4) (5 5) (6 6))))
	     (is "'UNDER',[1.0]'-'" "UNDER-")
  	     (is "'UNDER',[0.5]'-'" #2A((#\U #\N #\D #\E #\R) (#\- #\- #\- #\- #\-)))
  	     (is "'HELLO',[1.5]'.'" #2A((#\H #\.) (#\E #\.) (#\L #\.) (#\L #\.) (#\O #\.)))))
  (⍪ (has :titles ("Table" "Catenate First"))
     (ambivalent #'tabulate (catenate-on-first index-origin))
     (tests (is "⍪4" 4)
	    (is "⍪'MAKE'" #2A((#\M) (#\A) (#\K) (#\E)))
  	    (is "⍪3 4⍴⍳9" #2A((1 2 3 4) (5 6 7 8) (9 1 2 3)))
  	    (is "⍪2 3 4⍴⍳24" #2A((1 2 3 4 5 6 7 8 9 10 11 12)
  				 (13 14 15 16 17 18 19 20 21 22 23 24)))
	    (is "2⍪⍳4" #(2 1 2 3 4))
	    (is "(2 3⍴⍳6)⍪3" #2A((1 2 3) (4 5 6) (3 3 3)))
  	    (is "0⍪3 4⍴⍳9" #2A((0 0 0 0) (1 2 3 4) (5 6 7 8) (9 1 2 3)))
  	    (is "0⍪[2]3 4⍴⍳9" #2A((0 1 2 3 4) (0 5 6 7 8) (0 9 1 2 3)))
  	    (is "(3⍴5)⍪3 3⍴3" #2A((5 5 5) (3 3 3) (3 3 3) (3 3 3)))
  	    (is "(5 4⍴⍳6)⍪3 4⍴⍳9" #2A((1 2 3 4) (5 6 1 2) (3 4 5 6) (1 2 3 4)
  				      (5 6 1 2) (1 2 3 4) (5 6 7 8) (9 1 2 3)))
  	    (is "(3 6⍴⍳6)⍪[2]3 4⍴⍳9" #2A((1 2 3 4 5 6 1 2 3 4) (1 2 3 4 5 6 5 6 7 8)
  					 (1 2 3 4 5 6 9 1 2 3)))))
  (↑ (has :titles ("Mix" "Take"))
     (ambivalent (λωχ (mix-arrays (if axes (- (ceiling (first axes)) index-origin)
  				      (rank omega))
  				  omega))
  		 (section-array index-origin (quote (inws *value-meta*))))
     (inverse (monadic (λωχ (split-array omega *last-axis*))))
     (tests (is "↑2" 2)
	    (is "↑'a'" #\a)
	    (is "⍴1↑⍳3" #*1)
	    (is "↑(1)(1 2)(1 2 3)" #2A((1 0 0) (1 2 0) (1 2 3)))
  	    (is "↑[0.5](1)(1 2)(1 2 3)" #2A((1 1 1) (0 2 2) (0 0 3)))
  	    (is "↑(2 3⍴⍳5)(4 2⍴⍳8)" #3A(((1 2 3) (4 5 1) (0 0 0) (0 0 0))
  					((1 2 0) (3 4 0) (5 6 0) (7 8 0))))
  	    (is "↑(2 5⍴⍳9)(3 2 1)(4 3⍴⍳8)" #3A(((1 2 3 4 5) (6 7 8 9 1) (0 0 0 0 0) (0 0 0 0 0))
  					       ((3 2 1 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
  					       ((1 2 3 0 0) (4 5 6 0 0) (7 8 1 0 0) (2 3 4 0 0))))
  	    (is "↑[0.5](2 3⍴⍳5)(4 2⍴⍳8)" #3A(((1 1) (2 2) (3 0)) ((4 3) (5 4) (1 0))
  					     ((0 5) (0 6) (0 0)) ((0 7) (0 8) (0 0))))
  	    (is "↑[1.5](2 3⍴⍳5)(4 2⍴⍳8)" #3A(((1 2 3) (4 5 1) (0 0 0) (0 0 0))
  					     ((1 2 0) (3 4 0) (5 6 0) (7 8 0))))
	    (is "↑[0.5]2 3⍴(2 5⍴⍳9)(4 3 2 1)(4 3⍴⍳8)"
		#4A((((1 4 1) (1 4 1)) ((2 3 2) (2 3 2)) ((3 2 3) (3 2 3)) ((4 1 0) (4 1 0)) ((5 0 0) (5 0 0)))
		    (((6 0 4) (6 0 4)) ((7 0 5) (7 0 5)) ((8 0 6) (8 0 6)) ((9 0 0) (9 0 0)) ((1 0 0) (1 0 0)))
		    (((0 0 7) (0 0 7)) ((0 0 8) (0 0 8)) ((0 0 1) (0 0 1)) ((0 0 0) (0 0 0)) ((0 0 0) (0 0 0)))
		    (((0 0 2) (0 0 2)) ((0 0 3) (0 0 3)) ((0 0 4) (0 0 4)) ((0 0 0) (0 0 0)) ((0 0 0) (0 0 0)))))
	    (is "↑[1.5]2 3⍴(2 5⍴⍳9)(4 3 2 1)(4 3⍴⍳8)"
		#4A((((1 4 1) (2 3 2) (3 2 3) (4 1 0) (5 0 0)) ((6 0 4) (7 0 5) (8 0 6) (9 0 0) (1 0 0))
		     ((0 0 7) (0 0 8) (0 0 1) (0 0 0) (0 0 0)) ((0 0 2) (0 0 3) (0 0 4) (0 0 0) (0 0 0)))
		    (((1 4 1) (2 3 2) (3 2 3) (4 1 0) (5 0 0)) ((6 0 4) (7 0 5) (8 0 6) (9 0 0) (1 0 0))
		     ((0 0 7) (0 0 8) (0 0 1) (0 0 0) (0 0 0)) ((0 0 2) (0 0 3) (0 0 4) (0 0 0) (0 0 0)))))
	    (is "↑[2.5]2 3⍴(2 5⍴⍳9)(4 3 2 1)(4 3⍴⍳8)"
		#4A((((1 2 3 4 5) (6 7 8 9 1) (0 0 0 0 0) (0 0 0 0 0))
		     ((4 3 2 1 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
		     ((1 2 3 0 0) (4 5 6 0 0) (7 8 1 0 0) (2 3 4 0 0)))
		    (((1 2 3 4 5) (6 7 8 9 1) (0 0 0 0 0) (0 0 0 0 0))
		     ((4 3 2 1 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
		     ((1 2 3 0 0) (4 5 6 0 0) (7 8 1 0 0) (2 3 4 0 0)))))
  	    (is "↑2 2 2⍴(1)(1 2)(3 4)(1 2 3)" #4A((((1 0 0) (1 2 0)) ((3 4 0) (1 2 3)))
  						  (((1 0 0) (1 2 0)) ((3 4 0) (1 2 3)))))
	    (is "2↑2" #(2 0))
	    (is "3↑⍳9" #(1 2 3))
  	    (is "¯1↑⍳5" #(5))
  	    (is "3↑'abcdef'" "abc")
	    (is "8↑'a',1 2 3" #(#\a 1 2 3 #\  #\  #\  #\ ))
	    (is "8↑1 2,'ab',3 4" #(1 2 #\a #\b 3 4 0 0))
	    (is "3↑''" "   ")
	    (is "3↑⍬" #(0 0 0))
  	    (is "2 3 4↑4 5 6⍴⍳9" #3A(((1 2 3 4) (7 8 9 1) (4 5 6 7))
  				     ((4 5 6 7) (1 2 3 4) (7 8 9 1))))
  	    (is "2 ¯2 ¯2↑4 5 6⍴⍳9" #3A(((5 6) (2 3)) ((8 9) (5 6))))
  	    (is "5 ¯5↑(3 3⍴⍳9)∊1 2 3 4 8" #2A((0 0 1 1 1) (0 0 1 0 0) (0 0 0 1 0) (0 0 0 0 0) (0 0 0 0 0)))
	    (is "2 ¯2↑2 2⍴⍳4" #2A((1 2) (3 4)))
	    (is "2 ¯3↑3 4⍴⍳12" #2A((2 3 4) (6 7 8)))
	    (is "2 ¯5↑3 4⍴⍳12" #2A((0 1 2 3 4) (0 5 6 7 8)))
	    (is "1↑3 4⍴⍳12" #2A((1 2 3 4)))
	    (is "1↓3 4⍴⍳12" #2A((5 6 7 8) (9 10 11 12)))
  	    (is "1↑[1]2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3))))
  	    (is "1↑[2]2 3 4⍴⍳9" #3A(((1 2 3 4)) ((4 5 6 7))))
  	    (is "2↑[2]2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8)) ((4 5 6 7) (8 9 1 2))))
  	    (is "2↑[3]2 3 4⍴⍳9" #3A(((1 2) (5 6) (9 1)) ((4 5) (8 9) (3 4))))
	    (is "2 2↑[2 3]3 4 5⍴⍳9" #3A(((1 2) (6 7)) ((3 4) (8 9)) ((5 6) (1 2))))
	    (is "0↑4 5 6" #())
	    (is "0↑'a' 5 6" "")
	    (is "4↑(3 4⍴⍳12) 8 9" #(#2A((1 2 3 4) (5 6 7 8) (9 10 11 12)) 8 9
				    #2A((0 0 0 0) (0 0 0 0) (0 0 0 0))))
	    (is "6↑(⊂3 4⍴⍳9) 1 2" #(#0A#2A((1 2 3 4) (5 6 7 8) (9 1 2 3)) 1 2
				    #0A#2A((0 0 0 0) (0 0 0 0) (0 0 0 0))
				    #0A#2A((0 0 0 0) (0 0 0 0) (0 0 0 0))
				    #0A#2A((0 0 0 0) (0 0 0 0) (0 0 0 0))))
	    (is "⍴0↑3 4 5⍴5" #(0 4 5))
	    (is "⍴0 0↑3 4 5⍴5" #(0 0 5))
	    (is "3↑0↑⊂2 3⍴5" #(#2A((0 0 0) (0 0 0)) #2A((0 0 0) (0 0 0)) #2A((0 0 0) (0 0 0))))
	    (is "4↑0↑⊂2 2⍴(⊂2 2⍴⍳4) 2 3" #(#2A((#2A((0 0) (0 0)) 0) (0 #2A((0 0) (0 0))))
					   #2A((#2A((0 0) (0 0)) 0) (0 #2A((0 0) (0 0))))
					   #2A((#2A((0 0) (0 0)) 0) (0 #2A((0 0) (0 0))))
					   #2A((#2A((0 0) (0 0)) 0) (0 #2A((0 0) (0 0))))))
	    (is "2↑⍬ ⍬ ⍬ ⍬ ⍬" #(#() #()))
	    (is "8↑3⍴⊂0 0 0⍴1" #(#3A() #3A() #3A() #3A() #3A() #3A() #3A() #3A()))
	    (is "1↑2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3))))
	    (is "1 2↑2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8))))))
  (↓ (has :titles ("Split" "Drop"))
     (ambivalent (λωχ (split-array omega *last-axis*))
		 (section-array index-origin (quote (inws *value-meta*)) t))
     (inverse (monadic (λωχ (mix-arrays (if axes (- (ceiling (first axes)) index-origin)
					    (rank omega))
					omega))))
     (tests (is "↓5" 5)
	    (is "↓'b'" #\b)
	    (is "↓⍳5" #0A#(1 2 3 4 5))
	    (is "↓3 4⍴⍳9" #(#(1 2 3 4) #(5 6 7 8) #(9 1 2 3)))
  	    (is "↓[1]3 4⍴⍳9" #(#(1 5 9) #(2 6 1) #(3 7 2) #(4 8 3)))
	    (is "↓[3]3 4 5⍴⍳9" #2A((#(1 2 3 4 5) #(6 7 8 9 1) #(2 3 4 5 6) #(7 8 9 1 2))
				   (#(3 4 5 6 7) #(8 9 1 2 3) #(4 5 6 7 8) #(9 1 2 3 4))
				   (#(5 6 7 8 9) #(1 2 3 4 5) #(6 7 8 9 1) #(2 3 4 5 6))))
	    (is "↓2 2⍴⍳4" #(#(1 2) #(3 4)))
	    (is "↓↓2 2⍴⍳4" #0A#(#(1 2) #(3 4)))
	    (is "↓↓↓2 2⍴⍳4" #0A#0A#(#(1 2) #(3 4)))
	    (is "1↓2" #())
	    (is "2↓3" #())
  	    (is "2↓⍳9" #(3 4 5 6 7 8 9))
	    (is "4↓⍳9" #(5 6 7 8 9))
	    (is "3↓'abcdef'" "def")
  	    (is "2 2 2↓4 5 6⍴⍳9" #3A(((3 4 5 6) (9 1 2 3) (6 7 8 9))
  				     ((6 7 8 9) (3 4 5 6) (9 1 2 3))))
  	    (is "1↓[1]2 3 4⍴⍳9" #3A(((4 5 6 7) (8 9 1 2) (3 4 5 6))))
  	    (is "1↓[2]2 3 4⍴⍳9" #3A(((5 6 7 8) (9 1 2 3)) ((8 9 1 2) (3 4 5 6))))
  	    (is "2↓[2]2 3 4⍴⍳9" #3A(((9 1 2 3)) ((3 4 5 6))))
  	    (is "2↓[3]2 3 4⍴⍳9" #3A(((3 4) (7 8) (2 3)) ((6 7) (1 2) (5 6))))
	    (is "2 2↓[2 3]3 4 5⍴⍳9" #3A(((4 5 6) (9 1 2)) ((6 7 8) (2 3 4)) ((8 9 1) (4 5 6))))
  	    (is "¯2↓⍳9" #(1 2 3 4 5 6 7))
  	    (is "¯2 ¯2↓5 8⍴⍳9" #2A((1 2 3 4 5 6) (9 1 2 3 4 5) (8 9 1 2 3 4)))
  	    (is "4 5↓2 3⍴1" #2A())
	    (is "1↓2 3 4⍴⍳9" #3A(((4 5 6 7) (8 9 1 2) (3 4 5 6))))
	    (is "1 1↓2 3 4⍴⍳9" #3A(((8 9 1 2) (3 4 5 6))))))
  (⊂ (has :titles ("Enclose" "Partitioned Enclose"))
     (ambivalent (λωχ (if axes (re-enclose omega (aops:each (lambda (axis) (- axis index-origin))
  							    (if (arrayp (first axes))
								(first axes)
								(vector (first axes)))))
  			  (enclose omega)))
  		 (λωαχ (partitioned-enclose alpha omega *last-axis*)))
     (tests (is "⊂2" 2)
	    (is "(⊂2)=2" 1)
	    (is "(⊂'a')='a'" 1)
	    (is "⊂⍳5" #0A#(1 2 3 4 5))
  	    (is "1+⊂⍳5" #0A#(2 3 4 5 6))
	    (is "⊂'abc'" #0A"abc")
	    (is "≡⊂5 5" 2)
	    (is "≡⊂⊂5 5" 3)
	    (is "≡⊂⊂⊂5 5" 4)
  	    (is "1,⊂3 4⍴⍳7" #(1 #2A((1 2 3 4) (5 6 7 1) (2 3 4 5))))
  	    (is "⊂[3]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
  		#2A(("GRAY" "GOLD" "BLUE") ("SILK" "WOOL" "YARN")))
  	    (is "⊂[2]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
  		#2A(("GGB" "ROL" "ALU" "YDE") ("SWY" "IOA" "LOR" "KLN")))
  	    (is "⊂[1]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
  		#2A(("GS" "RI" "AL" "YK") ("GW" "OO" "LO" "DL") ("BY" "LA" "UR" "EN")))
  	    (is "⊂[2 3]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
  		#(#2A((#\G #\R #\A #\Y) (#\G #\O #\L #\D) (#\B #\L #\U #\E))
  		  #2A((#\S #\I #\L #\K) (#\W #\O #\O #\L) (#\Y #\A #\R #\N))))
  	    (is "⊂[1 3]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
  		#(#2A((#\G #\R #\A #\Y) (#\S #\I #\L #\K))
  		  #2A((#\G #\O #\L #\D) (#\W #\O #\O #\L))
  		  #2A((#\B #\L #\U #\E) (#\Y #\A #\R #\N))))
  	    (is "⊂[1 2]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
  		#(#2A((#\G #\G #\B) (#\S #\W #\Y)) #2A((#\R #\O #\L) (#\I #\O #\A))
  		  #2A((#\A #\L #\U) (#\L #\O #\R)) #2A((#\Y #\D #\E) (#\K #\L #\N))))
	    (is "1⊂2" #(#(2)))
	    (is "1⊂5" #(#(5)))
	    (is "1⊂⍳5" #(#(1) #(2) #(3) #(4) #(5)))
	    (is "2⊂⍳5" #(#() #(1) #() #(2) #() #(3) #() #(4) #() #(5)))
  	    (is "0 1 0 0 1 1 0 0 0⊂⍳9" #(#(2 3 4) #(5) #(6 7 8 9)))
  	    (is "0 1 0 0 1 1 0 0⊂4 8⍴⍳9"
  		#(#2A((2 3 4) (1 2 3) (9 1 2) (8 9 1)) #2A((5) (4) (3) (2))
  		  #2A((6 7 8) (5 6 7) (4 5 6) (3 4 5))))
  	    (is "0 1 0 1⊂[1]4 8⍴⍳9"
  		#(#2A((9 1 2 3 4 5 6 7) (8 9 1 2 3 4 5 6)) #2A((7 8 9 1 2 3 4 5))))
	    (is "2 0 1 3 0 2 0 1⊂'abcdefg'" #(#() "ab" "c" #() #() "de" #() "fg" #()))
	    (is "0 0 2 0 1⊂'abcdefg'" #(#() "cd" "efg"))))
  (⊆ (has :titles ("Nest" "Partition"))
     (ambivalent #'nest (λωαχ (partition-array alpha omega *last-axis*)))
     (inverse (monadic #'identity))
     (tests (is "⊆⍳3" #0A#(1 2 3))
	    (is "⊆1 2 (1 2 3)" #(1 2 #(1 2 3)))
	    (is "⊆'hello'" #0A"hello")
	    (is "⊆'hello' 'how' 'are' 'you'" #("hello" "how" "are" "you"))
	    (is "2⊆⍳3" #(#(1 2 3)))
	    (is "1 1 0⊆5 6 0" #(#(5 6)))
	    (is "1 1 2 2 2 3 3 3 3⊆⍳9" #(#(1 2) #(3 4 5) #(6 7 8 9)))
  	    (is "1 1 0 1⊆4 4 4⍴⍳9" #3A(((#(1 2) #(4)) (#(5 6) #(8)) (#(9 1) #(3)) (#(4 5) #(7)))
  				       ((#(8 9) #(2)) (#(3 4) #(6)) (#(7 8) #(1)) (#(2 3) #(5)))
  				       ((#(6 7) #(9)) (#(1 2) #(4)) (#(5 6) #(8)) (#(9 1) #(3)))
  				       ((#(4 5) #(7)) (#(8 9) #(2)) (#(3 4) #(6)) (#(7 8) #(1)))))
  	    (is "1 1 0 1⊆[2]4 4 4⍴⍳9" #3A(((#(1 5) #(2 6) #(3 7) #(4 8)) (#(4) #(5) #(6) #(7)))
  					  ((#(8 3) #(9 4) #(1 5) #(2 6)) (#(2) #(3) #(4) #(5)))
  					  ((#(6 1) #(7 2) #(8 3) #(9 4)) (#(9) #(1) #(2) #(3)))
  					  ((#(4 8) #(5 9) #(6 1) #(7 2)) (#(7) #(8) #(9) #(1)))))))
  (⊃ (has :titles ("Disclose" "Pick"))
     (ambivalent #'get-first-or-disclose (pick index-origin))
     (inverse (monadic (λωχ (if axes (error "Inverse [⊃ disclose] does not accept axis arguments.")
				(if (= 0 (rank omega))
				    omega (make-array nil :initial-contents omega))))))
     (tests (is "⊃3" 3)
	    (is "⊃⍳4" 1)
  	    (is "⊃⊂⍳4" #(1 2 3 4))
	    (is "⊃(⊂'test'),3" "test")
	    (is "⊃⍬" 0)
	    (is "' '=⊃''" 1)
	    (is "⊃¨⍴¨'one' 'a' 'two' 'three'" #(3 0 3 5))
	    (is "⊃⊂¨3⍴⊂⍳3" #0A#(1 2 3))
	    (is "1⊃5,⍬" 5)
	    (is "2⊃2 4 6 8" 4)
  	    (is "2⊃(1 2 3)(4 5 6)(7 8 9)" #(4 5 6))
  	    (is "2 2⊃(1 2 3)(4 5 6)(7 8 9)" 5)
	    (is "(⊂2 2)⊃3 4⍴⍳12" 6)
	    (is "4 (⊂1 3)⊃6⍴⊂3 4⍴⍳12" 3)
	    (is "4 (⊂1 3)⊃(5×⍳6)×6⍴⊂3 4⍴⍳12" 60)))
  (∩ (has :title "Intersection")
     (dyadic #'array-intersection)
     (tests (is "2∩⍳4" #(2))
	    (is "4 5 6∩4" #(4))
	    (is "'MIXTURE'∩'LATER'" "TRE")
  	    (is "'STEEL'∩'SABER'" "SEE")
  	    (is "1 4 8∩⍳5" #(1 4))
	    (is "'abc'∩'cde'" "c")
	    (is "'abc'∩'c'" "c")))
  (∪ (has :titles ("Unique" "Union"))
     (ambivalent #'unique #'array-union)
     (tests (is "∪3" #(3))
	    (is "∪1 2 3 4 5 1 2 8 9 10 11 7 8 11 12" #(1 2 3 4 5 8 9 10 11 7 12))
  	    (is "∪'MISSISSIPPI'" "MISP")
  	    (is "∪2 3 4⍴⍳12" #3A(((1 2 3 4) (5 6 7 8) (9 10 11 12))))
  	    (is "∪3 3 4⍴⍳24" #3A(((1 2 3 4) (5 6 7 8) (9 10 11 12))
				 ((13 14 15 16) (17 18 19 20) (21 22 23 24))))
	    (is "1∪1" #(1))
	    (is "2 3 4∪5" #(2 3 4 5))
	    (is "2∪⍳3" #(2 1 3))
  	    (is "3 10 14 18 11∪9 4 5 10 8 3" #(3 10 14 18 11 9 4 5 8))
  	    (is "'STEEL'∪'SABER'" "STEELABR")
  	    (is "'APRIL' 'MAY'∪'MAY' 'JUNE'" #("APRIL" "MAY" "JUNE"))))
  (⌽ (has :titles ("Reverse" "Rotate"))
     (ambivalent (λωχ (turn omega *last-axis*))
  		 (λωαχ (turn omega *last-axis* alpha)))
     (inverse (ambivalent #'identity :plain (λωαχ (turn omega *last-axis* (apply-scalar #'- alpha)))))
     (tests (is "⌽3" 3)
	    (is "⌽1 2 3 4 5" #(5 4 3 2 1))
  	    (is "⌽3 4⍴⍳9" #2A((4 3 2 1) (8 7 6 5) (3 2 1 9)))
	    (is "3⌽1" 1)
	    (is "3⌽⍳5" #(4 5 1 2 3))
  	    (is "2⌽3 4⍴⍳9" #2A((3 4 1 2) (7 8 5 6) (2 3 9 1)))
  	    (is "(2 2⍴1 2 3 4)⌽2 2 5⍴⍳9" #3A(((2 3 4 5 1) (8 9 1 6 7)) ((5 6 2 3 4) (2 7 8 9 1))))))
  (⊖ (has :titles ("Reverse First" "Rotate First"))
     (ambivalent (λωχ (turn omega *first-axis*))
  		 (λωαχ (turn omega *first-axis* alpha)))
     (inverse (ambivalent #'identity :plain (λωαχ (turn omega *first-axis* (apply-scalar #'- alpha)))))
     (tests (is "⊖4" 4)
	    (is "⊖1 2 3 4 5" #(5 4 3 2 1))
  	    (is "⊖3 4⍴⍳9" #2A((9 1 2 3) (5 6 7 8) (1 2 3 4)))
	    (is "2⊖5" 5)
	    (is "2⊖⍳6" #(3 4 5 6 1 2))
  	    (is "1⊖3 4⍴⍳9" #2A((5 6 7 8) (9 1 2 3) (1 2 3 4)))
  	    (is "(3 4 5⍴⍳4)⊖2 3 4 5⍴⍳9" #4A((((7 2 9 4 2) (6 4 8 6 1) (8 3 1 5 3) (7 5 9 7 2))
  					     ((9 4 2 6 4) (8 6 1 8 3) (1 5 3 7 5) (9 7 2 9 4))
  					     ((2 6 4 8 6) (1 8 3 1 5) (3 7 5 9 7) (2 9 4 2 6)))
  					    (((1 8 3 1 5) (3 7 5 9 7) (2 9 4 2 6) (4 8 6 1 8))
  					     ((3 1 5 3 7) (5 9 7 2 9) (4 2 6 4 8) (6 1 8 3 1))
  					     ((5 3 7 5 9) (7 2 9 4 2) (6 4 8 6 1) (8 3 1 5 3)))))))
  (⍉ (has :titles ("Transpose" "Permute"))
     (ambivalent (permute-array index-origin) (permute-array index-origin))
     (inverse (ambivalent (permute-array index-origin) :plain (permute-array index-origin)))
     (tests (is "⍉2" 2)
	    (is "⍉2 3 4⍴⍳9" #3A(((1 4) (5 8) (9 3)) ((2 5) (6 9) (1 4))
  				((3 6) (7 1) (2 5)) ((4 7) (8 2) (3 6))))
	    (is "1⍉5" 5)
	    (is "1⍉⍳3" #(1 2 3))
  	    (is "1 3 2⍉2 3 4⍴⍳9" #3A(((1 5 9) (2 6 1) (3 7 2) (4 8 3))
  				     ((4 8 3) (5 9 4) (6 1 5) (7 2 6))))))
  (/ (has :title "Replicate")
     (dyadic (λωαχ (expand-array alpha omega *last-axis* (quote (inws *value-meta*)) :compress-mode t)))
     (inverse (dyadic :plain (λωαχ (if (is-unitary omega)
				       ;; TODO: this inverse functionality is probably not complete
				       (expand-array alpha omega *last-axis*
						     (quote (inws *value-meta*)) :compress-mode t)
				       (error "Inverse [/ replicate] can only accept~a"
					      " a scalar right argument.")))))
     (tests (is "3/1" #*111)
	    (is "2/8" #(8 8))
	    (is "5/3" #(3 3 3 3 3))
  	    (is "1 0 1 0 1/⍳5" #(1 3 5))
  	    (is "3/⍳5" #(1 1 1 2 2 2 3 3 3 4 4 4 5 5 5))
  	    (is "3/⊂⍳5" #(#(1 2 3 4 5) #(1 2 3 4 5) #(1 2 3 4 5)))
	    (is "3/3 3⍴⍳9" #2A((1 1 1 2 2 2 3 3 3) (4 4 4 5 5 5 6 6 6) (7 7 7 8 8 8 9 9 9)))
	    (is "¯1/4 4⍴⍳16" #2A((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
  	    (is "1 ¯2 3 ¯4 5/3 5⍴⍳5" #2A((1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
  					 (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
  					 (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)))
	    (is "2/[2]3 4 5⍴⍳9" #3A(((1 2 3 4 5) (1 2 3 4 5) (6 7 8 9 1) (6 7 8 9 1)
				     (2 3 4 5 6) (2 3 4 5 6) (7 8 9 1 2) (7 8 9 1 2))
				    ((3 4 5 6 7) (3 4 5 6 7) (8 9 1 2 3) (8 9 1 2 3)
				     (4 5 6 7 8) (4 5 6 7 8) (9 1 2 3 4) (9 1 2 3 4))
				    ((5 6 7 8 9) (5 6 7 8 9) (1 2 3 4 5) (1 2 3 4 5)
				     (6 7 8 9 1) (6 7 8 9 1) (2 3 4 5 6) (2 3 4 5 6))))
	    (is "2 1 2 3/[2]3 4 5⍴⍳9" #3A(((1 2 3 4 5) (1 2 3 4 5) (6 7 8 9 1) (2 3 4 5 6)
					   (2 3 4 5 6) (7 8 9 1 2) (7 8 9 1 2) (7 8 9 1 2))
					  ((3 4 5 6 7) (3 4 5 6 7) (8 9 1 2 3) (4 5 6 7 8)
					   (4 5 6 7 8) (9 1 2 3 4) (9 1 2 3 4) (9 1 2 3 4))
					  ((5 6 7 8 9) (5 6 7 8 9) (1 2 3 4 5) (6 7 8 9 1)
					   (6 7 8 9 1) (2 3 4 5 6) (2 3 4 5 6) (2 3 4 5 6))))))
  (⌿ (has :title "Replicate First")
     (dyadic (λωαχ (expand-array alpha omega *first-axis* (quote (inws *value-meta*)) :compress-mode t)))
     (inverse (dyadic :plain (λωαχ (if (is-unitary omega)
				       ;; TODO: this inverse functionality is probably not complete
				       (expand-array alpha omega *first-axis*
						     (quote (inws *value-meta*)) :compress-mode t)
				       (error "Inverse [/ replicate] can only accept~a"
					      " a scalar right argument.")))))
     (tests (is "3⌿2" #(2 2 2))
	    (is "4⌿7 8" #(7 7 7 7 8 8 8 8))
	    (is "3⌿3 3⍴⍳9" #2A((1 2 3) (1 2 3) (1 2 3) (4 5 6) (4 5 6) (4 5 6) (7 8 9) (7 8 9) (7 8 9)))
	    (is "1 0 1 0 1⌿⍳5" #(1 3 5))
  	    (is "1 ¯2 3⌿3 5⍴⍳9" #2A((1 2 3 4 5) (0 0 0 0 0) (0 0 0 0 0)
  				    (2 3 4 5 6) (2 3 4 5 6) (2 3 4 5 6)))
  	    (is "1 ¯2 3 ¯4 5⌿[2]3 5⍴⍳5" #2A((1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
  					    (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
  					    (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)))))
  (\\ (has :title "Expand")
      (dyadic (λωαχ (expand-array alpha omega *last-axis* (quote (inws *value-meta*)))))
      (tests (is "4\\2" #(2 2 2 2))
      	     (is "3\\7" #(7 7 7))
      	     (is "1 ¯2 3 ¯4 5\\'.'" ".  ...    .....")
      	     (is "1 ¯2 2 0 1\\3+2 3⍴⍳6" #2A((4 0 0 5 5 0 6) (7 0 0 8 8 0 9)))
      	     (is "1 ¯2 2 0 1\\⍳3" #(1 0 0 2 2 0 3))
      	     (is "1 0 1\\[1]3+2 3⍴⍳6" #2A((4 5 6) (0 0 0) (7 8 9)))
      	     (is "1 ¯2 3 4\\[1]3 5⍴⍳9" #2A((1 2 3 4 5) (0 0 0 0 0) (0 0 0 0 0)
      	     				   (6 7 8 9 1) (6 7 8 9 1) (6 7 8 9 1)
      	     				   (2 3 4 5 6) (2 3 4 5 6) (2 3 4 5 6)
      	     				   (2 3 4 5 6)))
      	     (is "¯3\\0⍴⊂2 2⍴(⊂3 3⍴⍳6) 9 8 7" #(#2A((#2A((0 0 0) (0 0 0) (0 0 0)) 0) (0 0))
      	     					#2A((#2A((0 0 0) (0 0 0) (0 0 0)) 0) (0 0))
      	     					#2A((#2A((0 0 0) (0 0 0) (0 0 0)) 0) (0 0))))))
  (⍀ (has :title "Expand First")
     (dyadic (λωαχ (expand-array alpha omega *first-axis* (quote (inws *value-meta*)))))
     (tests (is "2⍀5" #(5 5))
     	    (is "2⍀1" #*11)
     	    (is "1 ¯2 3 ¯4 5⍀3" #(3 0 0 3 3 3 0 0 0 0 3 3 3 3 3))
     	    (is "1 0 1⍀3+2 3⍴⍳6" #2A((4 5 6) (0 0 0) (7 8 9)))))
  (⍋ (has :titles ("Grade Up" "Grade Up By"))
     (ambivalent (λω (grade omega index-origin (alpha-compare #'<=)))
  		 (λωα (grade (if (vectorp alpha)
  				 (index-of omega alpha index-origin)
  				 (array-grade alpha omega))
  			     index-origin (alpha-compare #'<))))
     (tests (is "⍋2" 1)
	    (is "⍋8 3 4 9 1 5 2" #(5 7 2 3 6 1 4))
  	    (is "⍋5 6⍴⍳16" #(1 4 2 5 3))
	    (is "'abcd'⍋,'d'" 1)
	    (is "'nsew'⍋'swwewnh'" #(6 1 4 2 3 5 7))
  	    (is "st←'aodjeignwug' ⋄ st[⍋st]" "adeggijnouw")
	    (is "{⍵[⍋⍵]}'abcABC012xyzXYZ789'" "012789ABCXYZabcxyz")
  	    (is "(2 5⍴'ABCDEabcde')⍋'ACaEed'" #(1 3 2 6 4 5))))
  (⍒ (has :titles ("Grade Down" "Grade Down By"))
     (ambivalent (λω (grade omega index-origin (alpha-compare #'>=)))
  		 (λωα (grade (if (vectorp alpha)
  				 (index-of omega alpha index-origin)
  				 (array-grade alpha omega))
  			     index-origin (alpha-compare #'>))))
     (tests (is "⍒3" 1)
	    (is "⍒6 1 8 2 4 3 9" #(7 3 1 5 6 4 2))
  	    (is "⍒5 6⍴⍳12" #(2 4 1 3 5))
	    (is "'abcd'⍒,'d'" 1)
	    (is "'nsew'⍒'swwewnh'" #(7 2 3 5 4 1 6))
  	    (is "st←'aodjeignwug' ⋄ st[⍒st]" "wuonjiggeda")
	    (is "{⍵[⍒⍵]}'abcABC012xyzXYZ789'" "zyxcbaZYXCBA987210")
  	    (is "(2 5⍴'ABCDEabcde')⍒'ACaEed'" #(5 4 6 2 3 1))))
  (⌹ (has :titles ("Matrix Inverse" "Matrix Divide"))
     (ambivalent #'matrix-inverse #'matrix-divide)
     (inverse (monadic #'matrix-inverse))
     (tests (is "⌹3" 1/3)
	    (is "⌹1 2 3 4" #(1/30 1/15 1/10 2/15))
  	    (is "⌹2 2⍴4 9 8 2" #2A((-1/32 9/64) (1/8 -1/16)))
  	    (is "⌹4 2⍴1 3 ¯4 9" #2A((3/14 -1/14 3/14 -1/14) (2/21 1/42 2/21 1/42)))
  	    (is "35 89 79⌹3 3⍴3 1 4 1 5 9 2 6 5" #(193/90 739/90 229/45))
  	    (is "(3 2⍴1 2 3 6 9 10)⌹3 3⍴1 0 0 1 1 0 1 1 1" #2A((1 2) (2 4) (6 4)))))
  (⊤ (has :title "Encode")
     (dyadic #'encode)
     (inverse (dyadic :plain #'decode))
     (tests (is "9⊤15" 6)
	    (is "6 2 8⊤12" #(0 1 4))
	    (is "1760 3 12⊤82" #(2 0 10))
  	    (is "16 16 16 16⊤100" #(0 0 6 4))
	    (is "0 12⊤8 64 256" #2A((0 5 21) (8 4 4)))
  	    (is "2 2 2 2 2⊤⍳5" #2A((0 0 0 0 0) (0 0 0 0 0) (0 0 0 1 1) (0 1 1 0 0) (1 0 1 0 1)))
  	    (is "16 16 16 16⊤2 2⍴100×⍳4"
  		#3A(((0 0) (0 0)) ((0 0) (1 1)) ((6 12) (2 9)) ((4 8) (12 0))))
	    (is "(2 2⍴16)⊤2 2⍴100 200 300 400"
		#4A((((6 12) (2 9)) ((6 12) (2 9))) (((4 8) (12 0)) ((4 8) (12 0)))))
	    (is "(2 2⍴16 8 8 16)⊤2 2⍴100×⍳4"
		#4A((((12 9) (5 2)) ((6 4) (2 1))) (((4 0) (4 0)) ((4 8) (12 0)))))
	    (is "(2 2⍴16 8 16 8)⊤2 2⍴100 200 300 400"
		#4A((((6 12) (2 9)) ((4 1) (5 2))) (((4 8) (12 0)) ((4 0) (4 0)))))
	    (is "(8 3⍴2 0 0 2 0 0 2 0 0 2 0 0 2 8 0 2 8 0 2 8 16 2 8 16)⊤83"
		#2A((0 0 0) (1 0 0) (0 0 0) (1 0 0) (0 0 0) (0 1 0) (1 2 5) (1 3 3)))))
  (⊥ (has :title "Decode")
     (dyadic #'decode)
     (inverse (dyadic :plain (λωα (encode omega alpha :inverse))))
     (tests (is "14⊥7" 7)
	    (is "6⊥12 50" 122)
	    (is "10⊥2 6 7 1" 2671)
  	    (is "32 14⊥7" 105)
  	    (is "1760 3 12⊥2 2 5" 101)
	    (is "1J1⊥⍳4" #C(5 9))
  	    (is "1760 3 12⊥3 3⍴1 2 1 5 0 2 2 3 7" #(98 75 67))
  	    (is "(3 3⍴1760 3 12)⊥3 3⍴2 2 5 1 4 9 6 6 7" #2A((90 126 295) (90 126 295) (90 126 295)))
	    (is "2⊥3 8⍴0 0 0 0 1 1 1 1 0 0 1 1 0 0 1 1 0 1 0 1 0 1 0 1" #(0 1 2 3 4 5 6 7))
	    (is "(3/⍪5 8 12)⊥3 3⍴2 2 5 1 4 9 6 6 7" #2A((61 76 177) (142 166 399) (306 342 835)))
	    (is "(3/⍪⍳4)⊥3 8⍴0 0 0 0 1 1 1 1 0 0 1 1 0 0 1 1 0 1 0 1 0 1 0 1"
		#2A((0 1 1 2 1 2 2 3) (0 1 2 3 4 5 6 7)
		    (0 1 3 4 9 10 12 13) (0 1 4 5 16 17 20 21)))
	    (is "(⍪2 10)⊥3 8⍴0 0 0 0 1 1 1 1 0 0 1 1 0 0 1 1 0 1 0 1 0 1 0 1"
		#2A((0 1 2 3 4 5 6 7) (0 1 10 11 100 101 110 111)))
	    (is "(3⍴2*8)⊥3 4 4⍴⍳39" #2A((69921 135714 201507 267300) (333093 398886 464679 530433)
					(596226 662019 727812 793605) (859398 925191 990984 1056777))))))

 (functions
  (with (:name :lexical-functions-special)
	(:tests-profile :title "Special Function Tests")
	(:demo-profile :title "Special Function Demos"
		       :description "These functions expose features of the language that aren't directly related to computing or transforming array values."))
  (⊢ (has :titles ("Identity" "Right"))
     (ambivalent #'identity (λωα (declare (ignore alpha))
				 omega))
     (inverse (ambivalent #'identity :plain (λωα (declare (ignore alpha)) omega)))
     (tests (is "⊢77" 77)
	    (is "55⊢77" 77)))
  (⊣ (has :titles ("Empty" "Left"))
     (ambivalent (λω omega)
		 (λωα (declare (ignore omega)) alpha))
     (tests (is "⊣77" 77)
	    (is "55⊣77" 55)))
  (⍕ (has :titles ("Format" "Format At Precision"))
     (ambivalent (format-array print-precision) (format-array print-precision))
     (tests (is "⍕3 4⍴⍳9" #2A((#\1 #\  #\2 #\  #\3 #\  #\4) (#\5 #\  #\6 #\  #\7 #\  #\8)
			      (#\9 #\  #\1 #\  #\2 #\  #\3)))
	    (is "⍕2 3 4⍴⍳9" #3A(((#\1 #\  #\2 #\  #\3 #\  #\4) (#\5 #\  #\6 #\  #\7 #\  #\8)
				 (#\9 #\  #\1 #\  #\2 #\  #\3))
				((#\4 #\  #\5 #\  #\6 #\  #\7) (#\8 #\  #\9 #\  #\1 #\  #\2)
				 (#\3 #\  #\4 #\  #\5 #\  #\6))))
	    (is "⍕⊂2 3 4⍴⍳9" #2A((#\  #\1 #\  #\2 #\  #\3 #\  #\4) (#\  #\5 #\  #\6 #\  #\7 #\  #\8)
				 (#\  #\9 #\  #\1 #\  #\2 #\  #\3) (#\  #\  #\  #\  #\  #\  #\  #\ )
				 (#\  #\4 #\  #\5 #\  #\6 #\  #\7) (#\  #\8 #\  #\9 #\  #\1 #\  #\2)
				 (#\  #\3 #\  #\4 #\  #\5 #\  #\6)))
	    (is "⍕3⍴⊂3 4⍴⍳9" #2A((#\  #\1 #\  #\2 #\  #\3 #\  #\4 #\  #\  #\1 #\  #\2 #\  #\3 #\  #\4 #\ 
				      #\  #\1 #\  #\2 #\  #\3 #\  #\4)
				 (#\  #\5 #\  #\6 #\  #\7 #\  #\8 #\  #\  #\5 #\  #\6 #\  #\7 #\  #\8 #\ 
				      #\  #\5 #\  #\6 #\  #\7 #\  #\8)
				 (#\  #\9 #\  #\1 #\  #\2 #\  #\3 #\  #\  #\9 #\  #\1 #\  #\2 #\  #\3 #\ 
				      #\  #\9 #\  #\1 #\  #\2 #\  #\3)))
	    (is "3⍕○3 4⍴⍳9" #2A((#\  #\3 #\. #\1 #\4 #\2 #\  #\  #\6 #\. #\2 #\8 #\3 #\  #\  #\9 #\. #\4
				     #\2 #\5 #\  #\1 #\2 #\. #\5 #\6 #\6)
				(#\1 #\5 #\. #\7 #\0 #\8 #\  #\1 #\8 #\. #\8 #\5 #\0 #\  #\2 #\1 #\. #\9
				     #\9 #\1 #\  #\2 #\5 #\. #\1 #\3 #\3)
				(#\2 #\8 #\. #\2 #\7 #\4 #\  #\  #\3 #\. #\1 #\4 #\2 #\  #\  #\6 #\. #\2
				     #\8 #\3 #\  #\  #\9 #\. #\4 #\2 #\5)))
	    (is "5⍕○3 4⍴⍳9" #2A((#\  #\3 #\. #\1 #\4 #\1 #\5 #\9 #\  #\  #\6 #\. #\2 #\8 #\3 #\1 #\9 #\ 
				     #\  #\9 #\. #\4 #\2 #\4 #\7 #\8 #\  #\1 #\2 #\. #\5 #\6 #\6 #\3 #\7)
				(#\1 #\5 #\. #\7 #\0 #\7 #\9 #\6 #\  #\1 #\8 #\. #\8 #\4 #\9 #\5 #\6 #\ 
				     #\2 #\1 #\. #\9 #\9 #\1 #\1 #\5 #\  #\2 #\5 #\. #\1 #\3 #\2 #\7 #\4)
				(#\2 #\8 #\. #\2 #\7 #\4 #\3 #\3 #\  #\  #\3 #\. #\1 #\4 #\1 #\5 #\9 #\ 
				     #\  #\6 #\. #\2 #\8 #\3 #\1 #\9 #\  #\  #\9 #\. #\4 #\2 #\4 #\7 #\8)))))
  (⍎ (has :title "Evaluate")
     (monadic (λω (eval (vex-program *april-idiom* '((state :print-output nil) (:space +workspace-name+))
				     (string omega)))))
     (tests (is "⍎'1+1'" 2)
	    (is "⍎'5','+3 2 1'" #(8 7 6))
	    (is "⍎'3'" 3)
	    (is "v←⍳3 ⋄ ⍎'v'" #(1 2 3))
	    (is "⍎¨'1+1' '2+2' '3+3'" #(2 4 6))))
  (← (has :title "Assign")
     (symbolic :special-lexical-form-assign)
     (tests (is "x←55 ⋄ x" 55)
	    (is "x←2 3 4⍴⍳9 ⋄ x[;1;]←7 ⋄ x" #3A(((7 7 7 7) (5 6 7 8) (9 1 2 3))
						((7 7 7 7) (8 9 1 2) (3 4 5 6))))
	    (is "(a b c)←1 2 3 ⋄ a b c" #(1 2 3))
	    (is "(a b c)←5 ⋄ ⊃+/a b c" 15)
	    (is "(a b c)←10 20 30 ⋄ a+b×c" 610)
	    (is "5+(a b c)←1 2 3" #(6 7 8))))
  (→ (has :title "Branch") 
     (symbolic :special-lexical-form-branch)
     (tests (is "x←1 ⋄ →1              ⋄ x×←11 ⋄ 1→⎕   ⋄ x×←3 ⋄ 2→⎕   ⋄ x×←5 ⋄ 3→⎕     ⋄ x×←7" 105)
	    (is "x←1 ⋄ →1+1            ⋄ x×←11 ⋄ 1→⎕   ⋄ x×←3 ⋄ 2→⎕   ⋄ x×←5 ⋄ 3→⎕     ⋄ x×←7" 35)
	    (is "x←1 ⋄ →2+3            ⋄ x×←11 ⋄ 1→⎕   ⋄ x×←3 ⋄ 2→⎕   ⋄ x×←5 ⋄ 3→⎕     ⋄ x×←7" 1155)
	    (is "x←1 ⋄ →0              ⋄ x×←11 ⋄ 1→⎕   ⋄ x×←3 ⋄ 2→⎕   ⋄ x×←5 ⋄ 3→⎕     ⋄ x×←7" 1155)
	    (is "x←1 ⋄ →three          ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7" 7)
	    (is "x←1 ⋄ (3-2)→two three ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7" 35)
	    (is "x←1 ⋄ 0→two three     ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7" 1155)))
  (∘ (has :title "Find Outer Product, Not Inner")
     (symbolic :outer-product-designator)))

 ;; APL's character-represented operators, which take one or two functions or arrays as input
 ;; and generate a function
 (operators
  (with (:name :lexical-operators-lateral)
	(:tests-profile :title "Lateral Operator Tests")
	(:demo-profile :title "Lateral Operator Demos"
		       :description "Lateral operators take a single operand function to their left, hence the name 'lateral.' The combination of operator and function yields another function which may be applied to one or two arguments depending on the operator."))
  (/ (has :title "Reduce")
     (lateral (with-derived-operands (axes left-glyph left-fn-dyadic)
		(let ((axes (if axes `(- ,(first axes) index-origin))))
		  `(operate-reducing ,left-fn-dyadic (string (quote ,left-glyph)) ,axes t))))
     (tests (is "+/1 2 3 4 5" 15)
	    (is "⊢/⍳5" 5)
	    (is "×/5" 5)
	    (is "÷/5" 5)
	    (is "+/3 4⍴⍳12" #(10 26 42))
	    (is "-/3 4⍴⍳12" #(-2 -2 -2))
	    (is "+/[1]3 4⍴⍳12" #(15 18 21 24))
	    (is "fn←{⍺+⍵} ⋄ fn/1 2 3 4 5" 15)
	    (is "⌊10_000×{⍺+÷⍵}/40/1" 16180)
	    (is "+/⍬" 0)
	    (is "-/⍬" 0)
	    (is "×/⍬" 1)
	    (is "÷/⍬" 1)
	    (is "</⍬" 0)
	    (is "≤/⍬" 1)
	    (is "⊤/⍬" 0)
	    (is "∪/⍬" #())
	    (is "f←+ ⋄ f/⍬" 0)
	    (is "g←÷ ⋄ g/⍬" 1)
	    (is "+/(1 2 3)(4 5 6)" #0A#(5 7 9))
	    (is "∩/¨(1 0 0) (1 1 0 1 0)⊂¨'abc' 'a|b|c'" #(#0A"abc" #0A""))
	    (is "4,/⍳12" #(#(1 2 3 4) #(2 3 4 5) #(3 4 5 6) #(4 5 6 7) #(5 6 7 8)
			   #(6 7 8 9) #(7 8 9 10) #(8 9 10 11) #(9 10 11 12)))
	    (is "⊃,/3 4+/¨⊂3 6⍴⍳9"
		#2A((6 9 12 15 10 14 18) (24 18 12 6 25 20 15) (15 18 21 24 22 26 30)))))
  (⌿ (has :title "Reduce First")
     (lateral (with-derived-operands (axes left-glyph left-fn-dyadic)
		(let ((axes (if axes `(- ,(first axes) index-origin))))
		  `(operate-reducing ,left-fn-dyadic (string (quote ,left-glyph)) ,axes))))
     (tests (is "+⌿3 4⍴⍳12" #(15 18 21 24))
	    (is "-⌿3 4⍴⍳12" #(5 6 7 8))
	    (is "{⍺×⍵+3}⌿3 4⍴⍳12" #(63 162 303 492))
	    (is "+⌿[2]3 4⍴⍳12" #(10 26 42))))
  (\\ (has :title "Scan")
      (lateral (with-derived-operands (axes left-fn-dyadic)
		 (let ((axes (if axes `(- ,(first axes) index-origin))))
		   `(operate-scanning ,left-fn-dyadic ,axes t))))
      (tests (is "÷\\5" 5)
	     (is "+\\1 2 3 4 5" #(1 3 6 10 15))
  	     (is "+\\3 4⍴⍳12" #2A((1 3 6 10) (5 11 18 26) (9 19 30 42)))
  	     (is "+\\[1]3 4⍴⍳12" #2A((1 2 3 4) (6 8 10 12) (15 18 21 24)))
	     (is "-\\2 3 4⍴⍳24" #3A(((1 -1 2 -2) (5 -1 6 -2) (9 -1 10 -2))
				    ((13 -1 14 -2) (17 -1 18 -2) (21 -1 22 -2))))))
  (⍀ (has :title "Scan First")
     (lateral (with-derived-operands (axes left-fn-dyadic)
		`(operate-scanning ,left-fn-dyadic ,(if axes `(- ,(first axes) index-origin)))))
     (tests (is "+⍀1 2 3 4 5" #(1 3 6 10 15))
  	    (is "+⍀3 4⍴⍳12" #2A((1 2 3 4) (6 8 10 12) (15 18 21 24)))
  	    (is "{⍺×⍵+3}⍀3 4⍴⍳12" #2A((1 2 3 4) (8 18 30 44) (63 162 303 492)))
  	    (is "+⍀[2]3 4⍴⍳12" #2A((1 3 6 10) (5 11 18 26) (9 19 30 42)))))
  (\¨ (has :title "Each")
      (lateral (with-derived-operands (axes left-fn-monadic left-fn-dyadic)
		 `(operate-each ,left-fn-monadic ,left-fn-dyadic)))
      (tests (is "⍳¨1 2 3" #(#(1) #(1 2) #(1 2 3)))
	     (is "{⍵÷3}¨10" 10/3)
	     (is "⍴⊢¨⊂1 2 3" #())
	     (is "1 {⍺+⍵÷3}¨10" 13/3)
  	     (is "3⍴¨1 2 3" #(#(1 1 1) #(2 2 2) #(3 3 3)))
  	     (is "3 4 5⍴¨3" #(#(3 3 3) #(3 3 3 3) #(3 3 3 3 3)))
  	     (is "1 ¯1⌽¨⊂⍳5" #(#(2 3 4 5 1) #(5 1 2 3 4)))
	     (is "3+¨3 3⍴⍳9" #2A((4 5 6) (7 8 9) (10 11 12)))
	     (is "⊃⍪/,/(⊂2 2⍴2 3 1 4){⍺+⍵××/⍴⍺}¨3 3⍴⍳9" #2A((6 7 10 11 14 15) (5 8 9 12 13 16)
							    (18 19 22 23 26 27) (17 20 21 24 25 28)
							    (30 31 34 35 38 39) (29 32 33 36 37 40)))
	     (is "(⍳3)⌽[1]¨⊂2 3 4⍴⍳9" #(#3A(((4 5 6 7) (8 9 1 2) (3 4 5 6)) ((1 2 3 4) (5 6 7 8) (9 1 2 3)))
					#3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 6 7) (8 9 1 2) (3 4 5 6)))
					#3A(((4 5 6 7) (8 9 1 2) (3 4 5 6)) ((1 2 3 4) (5 6 7 8) (9 1 2 3)))))
	     (is "(⍳3)⌽[2]¨⊂2 3 4⍴⍳9" #(#3A(((5 6 7 8) (9 1 2 3) (1 2 3 4)) ((8 9 1 2) (3 4 5 6) (4 5 6 7)))
					#3A(((9 1 2 3) (1 2 3 4) (5 6 7 8)) ((3 4 5 6) (4 5 6 7) (8 9 1 2)))
					#3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 6 7) (8 9 1 2) (3 4 5 6)))))
	     (is "(⍳3)⌽[3]¨⊂2 3 4⍴⍳9" #(#3A(((2 3 4 1) (6 7 8 5) (1 2 3 9)) ((5 6 7 4) (9 1 2 8) (4 5 6 3)))
					#3A(((3 4 1 2) (7 8 5 6) (2 3 9 1)) ((6 7 4 5) (1 2 8 9) (5 6 3 4)))
					#3A(((4 1 2 3) (8 5 6 7) (3 9 1 2)) ((7 4 5 6) (2 8 9 1) (6 3 4 5)))))
	     (is "(3 3⍴⊂3 3⍴⍳9)×¨3 3⍴⍳9" #2A((#2A((1 2 3) (4 5 6) (7 8 9))
						 #2A((2 4 6) (8 10 12) (14 16 18))
						 #2A((3 6 9) (12 15 18) (21 24 27)))
					     (#2A((4 8 12) (16 20 24) (28 32 36))
						 #2A((5 10 15) (20 25 30) (35 40 45))
						 #2A((6 12 18) (24 30 36) (42 48 54)))
					     (#2A((7 14 21) (28 35 42) (49 56 63))
						 #2A((8 16 24) (32 40 48) (56 64 72))
						 #2A((9 18 27) (36 45 54) (63 72 81)))))
	     (is "(1 1 1⍴⊂1 1 1)↓¨⊂3 3 3⍴⍳27" #3A(((#3A(((14 15) (17 18)) ((23 24) (26 27)))))))
	     (is "(1 0 0) (1 1 0 1 0)⊂¨'abc' 'a|b|c'"
		 #(#("abc") #("a" "|b" "|c")))))
  (⍨ (has :title "Commute")
     (lateral (with-derived-operands (axes left-fn-dyadic)
		;; Generate a function applying a function to arguments in reverse order, or duplicating a single argument.
		`(lambda (omega &optional alpha)
		   (funcall ,left-fn-dyadic (or alpha omega) omega))))
     (tests (is "5-⍨10" 5)
  	    (is "+⍨10" 20)
  	    (is "fn←{⍺+3×⍵} ⋄ 16 fn⍨8" 56)))
  (⌸ (has :title "Key")
     (lateral (with-derived-operands (axes left-fn-dyadic)
		`(operate-grouping ,left-fn-dyadic index-origin)))
     (tests (is "fruit←'Apple' 'Orange' 'Apple' 'Pear' 'Orange' 'Peach' 'Pear' 'Pear'
    quantities ← 12 3 2 6 8 16 7 3 ⋄ fruit {⍺ ⍵}⌸ quantities"
    	        #2A(("Apple" #(12 2)) ("Orange" #(3 8)) ("Pear" #(6 7 3)) ("Peach" #(16))))
  	    (is "fruit←'Apple' 'Orange' 'Apple' 'Pear' 'Orange' 'Peach' ⋄ {⍴⍵}⌸ fruit"
  		#2A((2) (2) (1) (1))))))

 (operators
  (with (:name :lexical-operators-pivotal)
	(:tests-profile :title "Pivotal Operator Tests")
	(:demo-profile :title "Pivotal Operator Demos"
		       :description "Pivotal operators are so called because they are entered between two operands. Depending on the operator, these operands may be functions or array values, with the combination yielding a new function."))
  (\. (has :title "Inner/Outer Product")
      (pivotal (with-derived-operands (right-fn-dyadic left-fn-dyadic left-fn-symbolic)
		 (if (eq :outer-product-designator left-fn-symbolic)
		     `(lambda (o a) (array-outer-product o a ,right-fn-dyadic))
		     `(lambda (a o) (array-inner-product o a ,right-fn-dyadic ,left-fn-dyadic)))))
      (tests (is "3+.×5" 15)
	     (is "2+.×3 4 5" 24)
	     (is "2 3 4+.×8 15 21" 145)
	     (is "2 3 4+.×3 3⍴3 1 4 1 5 9 2 6 5" #(17 41 55))
	     (is "⍴1 2 3+.+⍪1 2 3" #*1)
	     (is "(3 3⍴3 1 4 1 5 9 2 6 5)+.×2 3 4" #(25 53 42))
	     (is "{⍵ ⍵+.+⍵ ⍵} 3 3⍴⍳9" #0A#2A((4 8 12) (16 20 24) (28 32 36)))
	     (is "1 2 3 ÷.+ 1 2 3" 3)
	     (is "5∘.+5" 10)
	     (is "16∘.*⍳3" #(16 256 4096))
	     (is "4 5 6∘.+20 30 40 50" #2A((24 34 44 54) (25 35 45 55) (26 36 46 56)))
	     (is "1 2 3∘.-1 2 3" #2A((0 -1 -2) (1 0 -1) (2 1 0)))
	     (is "1 2 3∘.⍴1 2 3" #2A((#(1) #(2) #(3))
				     (#(1 1) #(2 2) #(3 3)) (#(1 1 1) #(2 2 2) #(3 3 3))))
	     (is "↑0 1 2∘.⌽↓3 3 ⍴⍳3" #3A(((1 2 3) (1 2 3) (1 2 3)) ((2 3 1) (2 3 1) (2 3 1))
					 ((3 1 2) (3 1 2) (3 1 2))))
	     (is "1 2 3∘.⍴⊂1 2 3" #(#(1) #(1 2) #(1 2 3)))
	     (is "1 2 3∘.⌽⊂1 2 3" #(#(2 3 1) #(3 1 2) #(1 2 3)))
	     (is "1 2 3∘.⌽⊂4 5 6 7" #(#(5 6 7 4) #(6 7 4 5) #(7 4 5 6)))
	     (is "1 2 3∘.⌽[1]⊂2 3 4⍴⍳9" #(#3A(((4 5 6 7) (8 9 1 2) (3 4 5 6)) ((1 2 3 4) (5 6 7 8) (9 1 2 3)))
					  #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 6 7) (8 9 1 2) (3 4 5 6)))
					  #3A(((4 5 6 7) (8 9 1 2) (3 4 5 6)) ((1 2 3 4) (5 6 7 8) (9 1 2 3)))))
	     (is "1 2 3∘.⌽[2]⊂2 3 4⍴⍳9" #(#3A(((5 6 7 8) (9 1 2 3) (1 2 3 4)) ((8 9 1 2) (3 4 5 6) (4 5 6 7)))
					  #3A(((9 1 2 3) (1 2 3 4) (5 6 7 8)) ((3 4 5 6) (4 5 6 7) (8 9 1 2)))
					  #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 6 7) (8 9 1 2) (3 4 5 6)))))
	     (is "1 2 3∘.⌽[3]⊂2 3 4⍴⍳9" #(#3A(((2 3 4 1) (6 7 8 5) (1 2 3 9)) ((5 6 7 4) (9 1 2 8) (4 5 6 3)))
					  #3A(((3 4 1 2) (7 8 5 6) (2 3 9 1)) ((6 7 4 5) (1 2 8 9) (5 6 3 4)))
					  #3A(((4 1 2 3) (8 5 6 7) (3 9 1 2)) ((7 4 5 6) (2 8 9 1) (6 3 4 5)))))
	     (is "(1 2 3) (2 3 4)∘.⌽[1]⊂3 3⍴⍳9" #(#2A((4 8 3) (7 2 6) (1 5 9))
						  #2A((7 2 6) (1 5 9) (4 8 3))))
	     (is "⍬∘.=⍬" #2A())
	     (is "''∘.=''" #2A())
	     (is "fn←{⍺×⍵+1} ⋄ 1 2 3∘.fn 4 5 6" #2A((5 6 7) (10 12 14) (15 18 21)))
	     (is "' ' { (A W)←{(⍵≠(≢⍵)⍴' ')/⍵}¨⍺ ⍵ ⋄ ((⍴A)=⍴W) ∧ ∧/(+/A∘.=W) = +/A∘.=A } 'dog'" #(0))
	     (is "⍴+.×⌿?2 30 30⍴1e10" #(30 30))))
  (∘ (has :title "Compose")
     (pivotal (with-derived-operands (right left right-glyph right-fn-monadic right-fn-dyadic
					    left-glyph left-fn-monadic left-fn-dyadic)
		(let ((right (if (or (not (symbolp right)) (not (fboundp right)))
				 right (symbol-function right)))
		      (is-fn-right (or (not (listp right)) (not (eql 'lambda (first right)))
				       (not (eql 'alambda (first right)))
				       (not (eql 'apl-compose (first right)))))
		      (right-may-be-dyadic (not (or left-fn-monadic left-fn-dyadic)))
		      (is-fn-left (or (not (listp left)) (not (eql 'lambda (first left)))
				      (not (eql 'alambda (first left)))
				      (not (eql 'apl-compose (first left)))))
		      (left (if (or (not (symbolp left)) (not (fboundp left)))
				left (symbol-function left))))
		  `(operate-composed ,(if is-fn-right right :fn)
				     ,right-fn-monadic ,(if right-may-be-dyadic right-fn-dyadic)
				     ,(if is-fn-left left :fn)
				     ,left-fn-monadic ,left-fn-dyadic
				     ,(or (and (listp left) (eql 'lambda (first left))
					       (= 1 (length (second left))))
					  (not left-fn-dyadic))))))
     (tests (is "fn←⍴∘⍴ ⋄ fn 2 3 4⍴⍳9" #(3))
  	    (is "⍴∘⍴ 2 3 4⍴⍳9" #(3))
  	    (is "⍴∘⍴∘⍴ 2 3 4⍴⍳9" #*1)
  	    (is "÷∘5 ⊢30" 6)
  	    (is "⌊10_000×(+∘*∘0.5) 4 16 25" #(56487 176487 266487))
  	    (is "fn←5∘- ⋄ fn 2" 3)
  	    (is "⌊0.5∘+∘*5 8 12" #(148 2981 162755))
  	    (is "⌊10_000×+∘÷/40/1" 16180)
  	    (is "fn←+/ ⋄ fn∘⍳¨2 5 8" #(3 15 36))
  	    (is "3 4⍴∘⍴2 4 5⍴9" #2A((2 4 5 2) (4 5 2 4) (5 2 4 5)))
	    (is "(2 3 4 5∘+) 5" #(7 8 9 10))))
  (⍤ (has :title "Rank / Atop")
     (pivotal (with-derived-operands (right right-fn-monadic right-fn-dyadic left-fn-monadic left-fn-dyadic)
		(if (or right-fn-monadic right-fn-dyadic)
		    `(operate-atop ,right-fn-monadic ,right-fn-dyadic ,left-fn-monadic)
		    `(operate-at-rank ,right ,left-fn-monadic ,left-fn-dyadic))))
     (tests (is "⊂⍤2⊢2 3 4⍴⍳9" #(#2A((1 2 3 4) (5 6 7 8) (9 1 2 3)) #2A((4 5 6 7) (8 9 1 2) (3 4 5 6))))
	    (is "⊂⍤1 0 2⊢2 3 4⍴⍳9" #2A((#(1 2 3 4) #(5 6 7 8) #(9 1 2 3)) (#(4 5 6 7) #(8 9 1 2) #(3 4 5 6))))
  	    (is "{(⊂⍋⍵)⌷⍵}⍤1⊢3 4 5⍴⍳9" #3A(((1 2 3 4 5) (1 6 7 8 9) (2 3 4 5 6) (1 2 7 8 9))
  					   ((3 4 5 6 7) (1 2 3 8 9) (4 5 6 7 8) (1 2 3 4 9))
  					   ((5 6 7 8 9) (1 2 3 4 5) (1 6 7 8 9) (2 3 4 5 6))))
  	    (is "10 20 30 40+⍤1⊢4 4 4⍴⍳16"
  		#3A(((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))
  		    ((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))
  		    ((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))
  		    ((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))))
	    (is "(3 4 5⍴⍳9)+∘⊃⍤2⊢⍳3" #3A(((2 3 4 5 6) (7 8 9 10 2) (3 4 5 6 7) (8 9 10 2 3))
					       ((4 5 6 7 8) (9 10 2 3 4) (5 6 7 8 9) (10 2 3 4 5))
					       ((6 7 8 9 10) (2 3 4 5 6) (7 8 9 10 2) (3 4 5 6 7))))
  	    (is "(⍳5)+⍤1⊢1 5⍴⍳5" #2A((2 4 6 8 10)))
	    (is "(⍳3)+⍤1⊢3 3⍴5" #2A((6 7 8) (6 7 8) (6 7 8)))
	    (is "(⍳3)+⍤0 1⊢3 3⍴5" #2A((6 6 6) (7 7 7) (8 8 8)))
	    (is "(⍪⍳3)+⍤1⊢3 3⍴5" #2A((6 6 6) (7 7 7) (8 8 8)))
  	    (is "fn←{⍺+2×⍵} ⋄ 15 25 35 fn⍤1⊢2 2 3⍴⍳8" #3A(((17 29 41) (23 35 47)) ((29 41 37) (19 31 43))))
	    (is "(-⍤÷) 4" -1/4)
	    (is "⌊3 (⋆⍤×) 4" 162754)))
  (⍥ (has :title "Over")
     (pivotal (with-derived-operands (right-fn-monadic left-fn-monadic left-fn-dyadic)
		(let ((omega (gensym)) (alpha (gensym)))
		  `(lambda (,omega &optional ,alpha)
		     (if ,alpha (funcall ,left-fn-dyadic (funcall ,right-fn-monadic ,omega)
					 (funcall ,right-fn-monadic ,alpha))
			 (funcall ,left-fn-monadic (funcall ,right-fn-monadic ,omega)))))))
     (tests (is "s←88 67 72 ⋄ w←15 35 22 ⋄ (w×s)÷⍥(+/)w" 5249/72)))
  (⍣ (has :title "Power")
     (pivotal (with-derived-operands (right left right-fn-dyadic left-fn-monadic
					    left-fn-dyadic left-op left-axes)
		(if right-fn-dyadic `(operate-until ,right-fn-dyadic ,(or left-fn-monadic left) ,left-fn-dyadic)
		    `(operate-to-power ,right ,(generate-function-retriever left-op left-axes)))))
     (tests (is "fn←{2+⍵}⍣3 ⋄ fn 5" 11)
  	    (is "{2+⍵}⍣3⊢9" 15)
  	    (is "2{⍺×2+⍵}⍣3⊢9" 100)
	    (is "{3×⍵}⍣(g←3)⊢5" 135)
  	    (is "fn←{2+⍵}⍣{10<⍺} ⋄ fn 2" 12)
  	    (is "fn←{2+⍵}⍣{10<⍵} ⋄ fn 2" 14)
  	    (is "fn←{⍵×2} ⋄ fn⍣3⊢4" 32)
	    (is "↓⍣2⊢2 2⍴⍳4" #0A#(#(1 2) #(3 4)))
	    (is "⌊1_000_000_000×2○⍣=1" 739085133)))
  (@ (has :title "At")
     (pivotal (with-derived-operands (right left right-fn-monadic left-fn-monadic left-fn-dyadic)
		`(operate-at ,(if (not (or left-fn-dyadic left-fn-monadic)) left)
			     ,(if (not right-fn-monadic) right)
			     ,left-fn-monadic ,left-fn-dyadic ,right-fn-monadic index-origin)))
     (tests (is "20 20@3 8⍳9" #(1 2 20 4 5 6 7 20 9))
	    (is "(0@2 4)⍳9" #(1 0 3 0 5 6 7 8 9))
	    (is "('*'@2)⍳5" #(1 #\* 3 4 5))
  	    (is "((2 5⍴0 1)@2 5) 5 5⍴⍳9" #2A((1 2 3 4 5) (0 1 0 1 0) (2 3 4 5 6)
  					     (7 8 9 1 2) (1 0 1 0 1)))
  	    (is "0@(×∘(3∘|)) ⍳9" #(0 0 3 0 0 6 0 0 9))
  	    (is "÷@3 5 ⍳9" #(1 2 1/3 4 1/5 6 7 8 9))
	    (is "⌽@(2∘|)⍳5" #(5 2 3 4 1))
	    (is "⌽@1 3 5⊢⍳5" #(5 2 3 4 1))
	    (is "⌽@1 3⊢4 5⍴⍳40" #2A((5 4 3 2 1) (6 7 8 9 10) (15 14 13 12 11) (16 17 18 19 20)))
	    (is "⌽@1⊢2 4 5⍴⍳40" #3A(((5 4 3 2 1) (10 9 8 7 6) (15 14 13 12 11) (20 19 18 17 16))
				    ((21 22 23 24 25) (26 27 28 29 30) (31 32 33 34 35) (36 37 38 39 40))))
  	    (is "{⍵×2}@{⍵>3}⍳9" #(1 2 3 8 10 12 14 16 18))
  	    (is "fn←{⍺+⍵×12} ⋄ test←{0=3|⍵} ⋄ 4 fn@test ⍳12" #(1 2 40 4 5 76 7 8 112 10 11 148))))
  (⌺ (has :title "Stencil")
     (pivotal (with-derived-operands (right left-fn-dyadic)
		`(operate-stenciling ,right ,left-fn-dyadic)))
     (tests (is "{⊂⍵}⌺(⍪3 2)⍳8" #(#(0 1 2) #(2 3 4) #(4 5 6) #(6 7 8)))
  	    (is "{⊂⍵}⌺(⍪5 2)⍳9" #(#(0 0 1 2 3) #(1 2 3 4 5) #(3 4 5 6 7) #(5 6 7 8 9) #(7 8 9 0 0)))
  	    (is "⊢∘⊂⌺2⍳8" #(#(1 2) #(2 3) #(3 4) #(4 5) #(5 6) #(6 7) #(7 8)))
  	    (is "⊢∘⊂⌺4⍳8" #(#(0 1 2 3) #(1 2 3 4) #(2 3 4 5) #(3 4 5 6)
			    #(4 5 6 7) #(5 6 7 8) #(6 7 8 0)))
  	    (is "⊢∘⊂⌺4⍳9" #(#(0 1 2 3) #(1 2 3 4) #(2 3 4 5) #(3 4 5 6)
  			    #(4 5 6 7) #(5 6 7 8) #(6 7 8 9) #(7 8 9 0)))
  	    (is "⊢∘⊂⌺(⍪4 2)⍳8" #(#(0 1 2 3) #(2 3 4 5) #(4 5 6 7) #(6 7 8 0)))
  	    (is "⊢∘⊂⌺(⍪6 2)⍳8" #(#(0 0 1 2 3 4) #(1 2 3 4 5 6) #(3 4 5 6 7 8) #(5 6 7 8 0 0)))
  	    (is "⊢⌺3 3⊢3 3⍴⍳9"
		#4A((((0 0 0) (0 1 2) (0 4 5)) ((0 0 0) (1 2 3) (4 5 6)) ((0 0 0) (2 3 0) (5 6 0)))
  		    (((0 1 2) (0 4 5) (0 7 8)) ((1 2 3) (4 5 6) (7 8 9)) ((2 3 0) (5 6 0) (8 9 0)))
  		    (((0 4 5) (0 7 8) (0 0 0)) ((4 5 6) (7 8 9) (0 0 0)) ((5 6 0) (8 9 0) (0 0 0)))))
	    (is "⊢⌺3 3⊢2 2⍴⍳12" #4A((((0 0 0) (0 1 2) (0 3 4)) ((0 0 0) (1 2 0) (3 4 0)))
				    (((0 1 2) (0 3 4) (0 0 0)) ((1 2 0) (3 4 0) (0 0 0)))))
	    (is "⊢⌺2 2⊢4 4⍴⍳16"
		#4A((((1 2) (5 6)) ((2 3) (6 7)) ((3 4) (7 8)))
		    (((5 6) (9 10)) ((6 7) (10 11)) ((7 8) (11 12)))
		    (((9 10) (13 14)) ((10 11) (14 15)) ((11 12) (15 16)))))
	    (is "⊢⌺(2 2⍴3 3 2 1)⊢4 4⍴⍳16"
		#4A((((0 0 0) (0 1 2) (0 5 6)) ((0 0 0) (1 2 3) (5 6 7)) ((0 0 0) (2 3 4) (6 7 8))
		     ((0 0 0) (3 4 0) (7 8 0))) (((0 5 6) (0 9 10) (0 13 14)) ((5 6 7) (9 10 11) (13 14 15))
		     ((6 7 8) (10 11 12) (14 15 16)) ((7 8 0) (11 12 0) (15 16 0)))))
  	    (is "{⊂⍺ ⍵}⌺3 3⊢3 3⍴⍳9"
		#2A((#(#(1 1) #2A((0 0 0) (0 1 2) (0 4 5))) #(#(1 0) #2A((0 0 0) (1 2 3) (4 5 6)))
  		      #(#(1 -1) #2A((0 0 0) (2 3 0) (5 6 0))))
  		    (#(#(0 1) #2A((0 1 2) (0 4 5) (0 7 8))) #(#(0 0) #2A((1 2 3) (4 5 6) (7 8 9)))
  		      #(#(0 -1) #2A((2 3 0) (5 6 0) (8 9 0))))
  		    (#(#(-1 1) #2A((0 4 5) (0 7 8) (0 0 0))) #(#(-1 0) #2A((4 5 6) (7 8 9) (0 0 0)))
  		      #(#(-1 -1) #2A((5 6 0) (8 9 0) (0 0 0))))))
	    (is ",∘⊂⌺3 3⊢3 3⍴⍳9" #3A(((1 1 #2A((0 0 0) (0 1 2) (0 4 5))) (1 0 #2A((0 0 0) (1 2 3) (4 5 6)))
				      (1 -1 #2A((0 0 0) (2 3 0) (5 6 0))))
				     ((0 1 #2A((0 1 2) (0 4 5) (0 7 8))) (0 0 #2A((1 2 3) (4 5 6) (7 8 9)))
				      (0 -1 #2A((2 3 0) (5 6 0) (8 9 0))))
				     ((-1 1 #2A((0 4 5) (0 7 8) (0 0 0))) (-1 0 #2A((4 5 6) (7 8 9) (0 0 0)))
				      (-1 -1 #2A((5 6 0) (8 9 0) (0 0 0))))))
  	    (is "+⌺(⍪6 2)⍳8" #2A((2 2 3 4 5 6) (1 2 3 4 5 6) (3 4 5 6 7 8) (3 4 5 6 -2 -2)))
	    (is ",⌺5⊢⍳6" #2A((2 0 0 1 2 3) (1 0 1 2 3 4) (0 1 2 3 4 5)
			     (0 2 3 4 5 6) (-1 3 4 5 6 0) (-2 4 5 6 0 0)))
	    (is "+/,{+/,⍵}⌺3 3⊢6 5 ⍴ ⍳5" 624)
	    (is "⊢⌺3 3⊢6 5⍴⍳5"
		#4A((((0 0 0) (0 1 2) (0 1 2)) ((0 0 0) (1 2 3) (1 2 3)) ((0 0 0) (2 3 4) (2 3 4))
		     ((0 0 0) (3 4 5) (3 4 5)) ((0 0 0) (4 5 0) (4 5 0)))
		    (((0 1 2) (0 1 2) (0 1 2)) ((1 2 3) (1 2 3) (1 2 3)) ((2 3 4) (2 3 4) (2 3 4))
		     ((3 4 5) (3 4 5) (3 4 5)) ((4 5 0) (4 5 0) (4 5 0)))
		    (((0 1 2) (0 1 2) (0 1 2)) ((1 2 3) (1 2 3) (1 2 3)) ((2 3 4) (2 3 4) (2 3 4))
		     ((3 4 5) (3 4 5) (3 4 5)) ((4 5 0) (4 5 0) (4 5 0)))
		    (((0 1 2) (0 1 2) (0 1 2)) ((1 2 3) (1 2 3) (1 2 3)) ((2 3 4) (2 3 4) (2 3 4))
		     ((3 4 5) (3 4 5) (3 4 5)) ((4 5 0) (4 5 0) (4 5 0)))
		    (((0 1 2) (0 1 2) (0 1 2)) ((1 2 3) (1 2 3) (1 2 3)) ((2 3 4) (2 3 4) (2 3 4))
		     ((3 4 5) (3 4 5) (3 4 5)) ((4 5 0) (4 5 0) (4 5 0)))
		    (((0 1 2) (0 1 2) (0 0 0)) ((1 2 3) (1 2 3) (0 0 0)) ((2 3 4) (2 3 4) (0 0 0))
		     ((3 4 5) (3 4 5) (0 0 0)) ((4 5 0) (4 5 0) (0 0 0))))))))

 (operators
  (with (:name :lexical-operators-unitary)
	(:tests-profile :title "Unitary Operator Tests")
	(:demo-profile :title "Unitary Operator Demos"
		       :description "Unitary operators take no operands and return a niladic function that returns a value; the use of unitary operators is to manifest syntax structures wherein depending on the outcome of some expressions, other expressions may or may not be evaluated, as with the [$ if] operator."))
  ($ (has :title "If")
     (unitary (lambda (workspace axes)
		(declare (ignore workspace))
		(cons 'apl-if axes)))
     (tests (is "$[1;2;3]" 2)
	    (is "$[0;2;3]" 3)
	    (is "x←5 ⋄ y←3 ⋄ $[y>2;x+←10;x+←20] ⋄ x" 15)
	    (is "3+$[5>6;1;7>8;2;3]" 6)
	    (is "{⍵+5}⍣$[3>2;4;5]⊢2" 22)
	    (is "{$[⍵>5;G←3⋄H←5⋄G+H;C←8⋄D←2⋄C×D]}¨3 7" #(16 8))
	    (is "{$[⍵<3;5;e←⍵+2⋄-{⍺⍺ ⍵} e]}¨⍳9" #(5 5 -5 -6 -7 -8 -9 -10 -11)))))

 ;; tests for general language functions not associated with a particular function or operator
 (test-set
  (with (:name :general-tests)
	(:tests-profile :title "General Tests")
	(:demo-profile :title "General Demos"
		       :description "These are demos of basic April language features."))
  (for "Scalar value." "5" 5)
  (for "Array value." "1 2 3" #(1 2 3))
  (for "String value." "'abcde'" "abcde")
  (for "Empty string value." "''" "")
  (for "Scalar values operated upon." "3×3" 9)
  (for "Array and scalar values operated upon." "5+1 2 3" #(6 7 8))
  (for "Two array values operated upon." "4 12 16÷2 3 4" #(2 4 4))
  (for "Monadic operation upon nested vectors." "-(1 2 3)(4 5 6)" #(#0A#(-1 -2 -3) #0A#(-4 -5 -6)))
  (for "Dyadic operation upon nested vectors."
       "((1 2 3)(4 5 6))×(7 8 9)(10 11 12)" #(#(7 16 27) #(40 55 72)))
  (for "Scalar operation with axes on arrays of differing ranks."
       "1 2 3+[1]3 4⍴⍳9" #2A((2 3 4 5) (7 8 9 10) (12 4 5 6)))
  (for "As above on the second axis." "1 2 3 4+[2]3 4⍴⍳9" #2A((2 4 6 8) (6 8 10 12) (10 3 5 7)))
  (for "Arithmetic with scalar and high-rank singleton array." "3+1 1 1 1⍴4" #4A((((7)))))
  (for "Boolean operation with vector of left arguments and enclosed vector on the right."
       "3 4=⊂3 4 5" #(#(1 0 0) #(0 1 0)))
  (for "Value assigned to a variable." "x←9" 9)
  (for "Value assigned to a variable and operated upon." "3+x←9" 12)
  (for "Two statements on one line separated by a [⋄ diamond] character."
       "a←9 ⋄ a×2 3 4" #(18 27 36))
  (for "Quote marks in string escaped using traditional double-quote method." "'''abc'''" "'abc'")
  (for "Quote marks in string escaped with backslashes." "'\\'abc\\''" "'abc'")
  (for "Basic function definition and use, with comments delineated by the [⍝ lamp] character."
       "⍝ This code starts with a comment.
    f1←{⍵+3} ⋄ f2←{⍵×2} ⍝ A comment after the functions are defined.
    ⍝ This is another comment.
    v←⍳3 ⋄ f2 f1 v,4 5"
       #(8 10 12 14 16))
  (for "One-line assignment followed by comment." "f←1 2 3 ⍝ Comment follows." #(1 2 3))
  (for "Monadic inline function." "{⍵+3} 3 4 5" #(6 7 8))
  (for "Dyadic inline function." "1 2 3 {⍺×⍵+3} 3 4 5" #(6 14 24))
  (for "Vector of input variables and discrete values processed within a function."
       "fn←{3+⍵} ⋄ {fn 8 ⍵} 9" #(11 12))
  (for "Definition and use of n-argument function."
       "fn←{[x;y;z] x+y×z} ⋄ fn[4;5;6]" 34)
  (for "Inline n-argument function."
       "{[a;b;c;d](a-c)×b/d}[7;4;2;⍳3]" #(5 5 5 5 10 10 10 10 15 15 15 15))
  (for "Variable-referenced values, including an element within an array, in a vector."
       "a←9 ⋄ b←2 3 4⍴⍳9 ⋄ 1 2 a 3 (b[1;2;1])" #(1 2 9 3 5))
  (for "Index of inline vector." "5 6 7 8[2]" 6)
  (for "Index of inline vector starting with index of another inline vector."
       "a←9 10 11 ⋄ 1 2 a[2] 3 4 5 6[3]" 4)
  (for "Index of inline nested vector." "(1 2 3 4) 15[1]" #0A#(1 2 3 4))
  (for "Manifold left-associative indexing." "'a' 2[1] 2[1] 2[1] 2[1] 2[1]" #\a)
  (for "Index of vector of strings." "'abc' 'def' 'ghi'[2]" #0A"def")
  (for "Indexing with empty vectors to create n-dimensional empty arrays."
       "a←3 4⍴⍳12 ⋄ ⍴a[⍬;]" #(0 4))
  (for "Indexing with variables." "x←3 3⍴⍳9 ⋄ y←1 ⋄ x[;y]" #(1 4 7))
  (for "Application of functions to indexed array elements."
       "g←2 3 4 5 ⋄ 9,g[2],3 4" #(9 3 3 4))
  (for "Assignment of an element within an array."
       "a←2 3⍴⍳9 ⋄ a[1;2]←20 ⋄ a" #2A((1 20 3) (4 5 6)))
  (for "Assignment of enclosed array to multiple indices of an array."
       "a←⍳9 ⋄ a[3 6]←⊂9 8 ⋄ a" #(1 2 #(9 8) 4 5 #(9 8) 7 8 9))
  (for "Assignment to copy of an array." "a←3 3⍴⍳9 ⋄ b←a ⋄ b[1;]←0 ⋄ a,b"
       #2A((1 2 3 0 0 0) (4 5 6 4 5 6) (7 8 9 7 8 9)))
  (for "Strand assignment of variables including a system variable."
       "(x ⎕IO y)←10 0 2 ⋄ x+y×⍳5" #(10 12 14 16 18))
  (for "Strand assignment of nested scalar variable."
       "⎕IO←1 ⋄ (a b c)←⊂3 3⍴1 ⋄ ⊃+/a b c" #2A((3 3 3) (3 3 3) (3 3 3)))
  (for "Strand assignment of variables without parentheses." "a b c←4 5 6 ⋄ a×b,c" #(20 24))
  (for "Strand assignment with nesting." "d (e f)←7 (8 9) ⋄ e⍴d×f" #(63 63 63 63 63 63 63 63))
  (for "Selection from an array with multiple elided dimensions."
       "(2 3 3 4 5⍴⍳9)[2;;3;;2]" #2A((6 2 7 3) (3 8 4 9) (9 5 1 6)))
  (for "Selection from an array with multi-index, array and elided dimensions."
       "(3 3 3⍴⍳27)[1 2;2 2⍴⍳3;]" #4A((((1 2 3) (4 5 6)) ((7 8 9) (1 2 3)))
				      (((10 11 12) (13 14 15)) ((16 17 18) (10 11 12)))))
  (for "Selection from witthin an array with spaces in axis specification."
       "(3 4⍴⍳12)[ ;4 3 ]" #2A((4 3) (8 7) (12 11)))
  (for "Elided assignment."
       "a←2 3 4⍴⍳9 ⋄ a[2;;3]←0 ⋄ a" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 0 7) (8 9 0 2) (3 4 0 6))))
  (for "Assignment from an array to an area of an array with the same shape."
       "x←8 8⍴0 ⋄ x[2+⍳3;3+⍳4]←3 4⍴⍳9 ⋄ x" #2A((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)
					       (0 0 0 1 2 3 4 0) (0 0 0 5 6 7 8 0)
					       (0 0 0 9 1 2 3 0) (0 0 0 0 0 0 0 0)
					       (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)))
  (for "Elision and indexed array elements."
       "(6 8⍴⍳9)[1 4;]" #2A((1 2 3 4 5 6 7 8) (7 8 9 1 2 3 4 5)))
  (for "As above but more complex."
       "(6 8 5⍴⍳9)[1 4;;2 1]" #3A(((2 1) (7 6) (3 2) (8 7) (4 3) (9 8) (5 4) (1 9))
				  ((5 4) (1 9) (6 5) (2 1) (7 6) (3 2) (8 7) (4 3))))
  (for "Indices of indices."
       "(6 8 5⍴⍳9)[1 4;;2 1][1;2 4 5;]" #2A((7 6) (8 7) (4 3)))
  (for "Array as array index."
       "(10+⍳9)[2 3⍴⍳9]" #2A((11 12 13) (14 15 16)))
  (for "Sub-coordinates of nested arrays." "(3 4⍴⍳9)[(1 2)(3 1)]" #(2 9))
  (for "Choose indexing of nested array sub-coordinates."
       "(3 4⍴⍳9)[2 2⍴⊂(2 3)]" #2A((7 7) (7 7)))
  (for "Reach indexing of components within sub-arrays."
       "(2 3⍴('JAN' 1)('FEB' 2)('MAR' 3)('APR' 4)('MAY' 5)('JUN' 6))[((2 3)1)((1 1)2)]" #(#0A"JUN" 1))
  (for "Assignment by function." "a←3 2 1 ⋄ a+←5 ⋄ a" #(8 7 6))
  (for "Assignment by function at index." "a←3 2 1 ⋄ a[2]+←5 ⋄ a" #(3 7 1))
  (for "Elided assignment of applied function's results."
       "a←2 3 4⍴⍳9 ⋄ a[2;;3]+←10 ⋄ a" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 16 7) (8 9 11 2) (3 4 15 6))))
  (for "Assignment by function result of array after after function applied."
       "q←3×1-⍨3 3⍴⍳4 ⋄ p←4 4⍴1 ⋄ p[⍳3;⍳3]×←q=0 ⋄ p" #2A((1 0 0 1) (0 1 0 1) (0 0 1 1) (1 1 1 1)))
  (for "Operation over portions of an array."
       "a←4 8⍴⍳9 ⋄ a[2 4;1 6 7 8]+←10 ⋄ a"
       #2A((1 2 3 4 5 6 7 8) (19 1 2 3 4 15 16 17)
	   (8 9 1 2 3 4 5 6) (17 8 9 1 2 13 14 15)))
  (for "Assignment of array element referenced by [⌷ index] function."
       "x←3 3⍴⍳9 ⋄ (2 3⌷x)←33 ⋄ x" #2A((1 2 3) (4 5 33) (7 8 9)))
  (for "Assignment of array element referenced by [⌷ index] function to different type."
       "x←3 3⍴⍳9 ⋄ (1 2⌷x)←'a' ⋄ x" #2A((1 #\a 3) (4 5 6) (7 8 9)))
  (for "Selective assignment of vector portion to value by take function."
       "x←⍳8 ⋄ (3↑x)←20 ⋄ x" #(20 20 20 4 5 6 7 8))
  (for "Selective assignment of vector portion to sub-vector by take function."
       "x←⍳8 ⋄ (3↑x)←20 21 22 ⋄ x" #(20 21 22 4 5 6 7 8))
  (for "Selective assignment of matrix portion to value by drop function."
       "x←4 5⍴⍳20 ⋄ (2 3↓x)←0 ⋄ x" #2A((1 2 3 4 5) (6 7 8 9 10) (11 12 13 0 0) (16 17 18 0 0)))
  (for "Selective assignment of matrix portion to sub-matrix by drop function."
       "x←4 5⍴⍳20 ⋄ (2 3↓x)←2 2⍴-⍳4 ⋄ x" #2A((1 2 3 4 5) (6 7 8 9 10) (11 12 13 -1 -2) (16 17 18 -3 -4)))
  (for "Selective assignment of matrix element by pick function."
       "x←3 4⍴⍳12 ⋄ ((⊂2 3)⊃x)←50 ⋄ x" #2A((1 2 3 4) (5 6 50 8) (9 10 11 12)))
  (for "Selective assignment of array elements by compress function."
       "x←6 8⍴⍳9 ⋄ ((30>+⌿x)/x)←0 ⋄ x" #2A((1 2 3 0 0 0 0 8) (9 1 2 0 0 0 0 7) (8 9 1 0 0 0 0 6)
					   (7 8 9 0 0 0 0 5) (6 7 8 0 0 0 0 4) (5 6 7 0 0 0 0 3)))
  (for "Selective assignment of elements within nested array by take function."
       "x←3⍴⊂⍳4 ⋄ (1↑x[1])←99 ⋄ x" #(99 #(1 2 3 4) #(1 2 3 4)))
  (for "Selective assignment of elements within nested array by pick function."
       "x←3⍴⊂⍳4 ⋄ (1↑⊃x[1])←99 ⋄ x" #(#(99 2 3 4) #(1 2 3 4) #(1 2 3 4)))
  (for "Multiple assignment with selective assignment in midstream."
       "a←⍳5 ⋄ b←(3⊃a)←30 ⋄ a b" #(#(1 2 30 4 5) 30))
  (for "Print the result of a function applied to assignment." "⎕←⍴x←1 2 3 ⋄ x" #(1 2 3))
  (for "Assignment of dynamic variable within function."
       "jje←3 ⋄ bob←{jje+←⍵ ⋄ jje} ⋄ bob 5" 8)
  (for "Index of variable with value assigned inside its own index."
       "y[⍋y←1 8 4 2]" #(1 2 4 8))
  (for "Inline pivotal operation-derived function expression."
       "1 2 3 (∘.+) 4 5 6" #2A((5 6 7) (6 7 8) (7 8 9)))
  ;; (for "Composed pivotal operation-derived function expression."
  ;;     "1 2 3∘(×.+)⊢4 5 6" 315)
  ;; (for "Multiple composed pivotal operations called in sequence."
  ;;      "(4 5 6∘(∘.×)) (1 2 3∘(∘.+)) 10 20 30"
  ;;      #3A(((44 84 124) (48 88 128) (52 92 132))
  ;; 	   ((55 105 155) (60 110 160) (65 115 165))
  ;; 	   ((66 126 186) (72 132 192) (78 138 198))))
  (for "Two-element monadic atop function train." "(↓⌽)4 5⍴⍳20"
       #(#(5 4 3 2 1) #(10 9 8 7 6) #(15 14 13 12 11) #(20 19 18 17 16)))
  (for "Two-element dyadic atop function train." "'mississippi'(⍸∊)'sp'" #(3 4 6 7 9 10))
  (for "Basic three-element monadic fork function train." "(-,÷)5" #(-5 1/5))
  (for "Three-element monadic fork function train with inline function." "(+ {⍺×⍵} -)5" -25)
  (for "Three-element monadic fork function train with variable-referenced and inline functions."
       "f←{⍺×⍵} ⋄ ({3+⍵} f -)5" -40)
  (for "Three-element fork function train with referenced functions at ends."
       "of←{⍵+1} ⋄ ef←{⍵+2} ⋄ (of , ef) 5" #(6 7))
  (for "Three-element averaging fork function train with operator-derived function."
       "(+/÷≢)⍳12" 13/2)
  (for "Five-element monadic fork function train." "(!⍴-,÷)3" #(-3 1/3 -3 1/3 -3 1/3))
  (for "Five-element monadic fork function train with second argument value at end."
       "(3 5⍴-,÷)5" #2A((-5 1/5 -5 1/5 -5) (1/5 -5 1/5 -5 1/5) (-5 1/5 -5 1/5 -5)))
  (for "Three-element dyadic fork function train."
       "' ' (≠⊆⊢) ' one two  three'" #("one" "two" "three"))
  (for "Three-element dyadic fork function train with left argument value."
       "(⍳8) (12>+) (⍳8)⋆1.2" #(1 1 1 1 1 0 0 0))
  (for "Three-element monadic fork function train including operator-composed function."
       "(1+-∘÷) 4" 3/4)
  (for "Five-element dyadic fork function train."
       "' ' (∊{⍺,⍵[⍺],⍵}≠⊆⊢) ' one two  three'" #(1 "one" "one" "two" "three"))
  (for "Five-element monadic fork function train including lateral and pivotal function compositions."
       "(⊢⌽⍨¯1+⍳∘≢)5 5⍴⍳25" #2A((1 2 3 4 5) (7 8 9 10 6) (13 14 15 11 12) (19 20 16 17 18) (25 21 22 23 24)))
  (for "Recursive function." "refn←{A←⍵-1 ⋄ $[A≥0;A,refn A;0]} ⋄ refn 5" #(4 3 2 1 0 0))
  (for "Lateral operator definition." "lop←{8 ⍺⍺ 5×2+⍵} ⋄ × lop 5" 280)
  (for "Pivotal operator definition." "pop←{(⍵ ⍵⍵ ⍺) ⍺⍺ (⍺ ⍵⍵ ⍵)} ⋄ 2-pop≤⊢3" -1)
  (for "Lateral recursive operator definition with reference to composed function."
       "rlop←{$[⍵<2000;⍵,∇ 3 ⍺⍺ 2×2+⊃⍵;⍵]} ⋄ × rlop 5" #(5 42 264 1596 9588))
  (for "Lateral recursive operator definition with self-reference."
       "rlop←{$[⍵<2000;⍵,⍺⍺ ∇∇ 3 ⍺⍺ 2×2+⊃⍵;⍵]} ⋄ × rlop 5" #(5 42 264 1596 9588))
  (for "Inline lateral operator." "× {8 ⍺⍺ 5×2+⍵} 5" 280)
  (for "Inline pivotal operator." "2-{(⍵ ⍵⍵ ⍺) ⍺⍺ (⍺ ⍵⍵ ⍵)}≤⊢3" -1)
  (for "Inline lateral operator with left argument." "3 +{⍺ ⍺⍺ ⍵} 4" 7)
  (for "Inline pivotal operator with unused left operand." "3 +{⍺ ⍵⍵ ⍵}× 4" 12)
  (for "Function applied to result of pivotal operator." "∊∘.+⍨10 2" #(20 12 12 4))
  (for "Lateral operator within a defined function." "fn←{÷ {⍺⍺ ⍵} 1+⍵} ⋄ - fn 2" -1/3)
  (for "Inline pivotal operator in parentheses with internal ⋄ breaks." "3 (+{⍺⍺ 2 ⋄ ⍺ ⍵⍵ ⍵}÷) 4" 3/4)
  (for "Operator composition of function within defined operator."
       "filter←{(⍺⍺¨⍵)/⍵} ⋄ {2|⍵} filter ⍳20" #(1 3 5 7 9 11 13 15 17 19))
  (for "Array processing function applied over nested array."
       "{((5=¯1↑⍵)+1)⊃¯1 (⊂⍵)}¨(⊂1 5),⍨3⍴⊂⍳4" #(-1 -1 -1 #0A#(1 5)))
  (for "Indexed element of above array."
       "{⍵,≡⍵}4⌷{((5=¯1↑⍵)+1)⊃¯1 (⊂⍵)}¨(⊂1 5),⍨3⍴⊂⍳4" #(#0A#(1 5) 3))
  (for "Fibonacci sequence generated using [∇ self] for self-reference within a function."
       "{$[(⍵=1)∨⍵=2;1;(∇ (⍵-2))+∇ (⍵-1)]}¨⍳7" #(1 1 2 3 5 8 13))
  (for "Glider 1." "(3 3⍴⍳9)∊1 2 3 4 8" #2A((1 1 1) (1 0 0) (0 1 0)))
  (for "Glider 2." "3 3⍴⌽⊃∨/1 2 3 4 8=⊂⍳9" #2A((0 1 0) (0 0 1) (1 1 1))))

 (test-set
  (with (:name :system-variable-function-tests)
	(:tests-profile :title "System Variable and Function Tests")
	(:demo-profile :title "System Variable and Function Demos"
		       :description "Demos illustrating the use of system variables and functions."))
  (for "Setting the index origin." "a←⍳3 ⋄ ⎕io←0 ⋄ a,⍳3" #(1 2 3 0 1 2))
  (for-printed "Setting the print precision." "⎕pp←3 ⋄ ⎕io←1 ⋄ a←⍕*⍳3 ⋄ ⎕pp←6 ⋄ a,'  ',⍕*⍳3"
	       "2.72 7.39 20.1  2.71828 7.38906 20.0855")
  (for "Alphabetical and numeric vectors." "⎕pp←10 ⋄ ⎕a,⎕d" "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
  (for "Seven elements in the timestamp vector." "⍴⎕ts" #(7)))

 (test-set
  (with (:name :function-inversion-tests)
 	(:tests-profile :title "Function Inversion Tests")
 	(:demo-profile :title "Function Inversion Demos"
 		       :description "Demos of the negative-indexed [⍣ power] operator that inverts simple functions passed to it."))
  (for "Inverse addition."       "(3+⍣¯1⊢8),((3∘+)⍣¯1⊢8),(+∘3)⍣¯1⊢8" #(5 5 5))
  (for "Inverse subtraction."    "(3-⍣¯1⊢8),((3∘-)⍣¯1⊢8),(-∘3)⍣¯1⊢8" #(-5 -5 11))
  (for "Inverse multiplication." "(3×⍣¯1⊢8),((3∘×)⍣¯1⊢8),(×∘3)⍣¯1⊢8" #(8/3 8/3 8/3))
  (for "Inverse division."       "(3÷⍣¯1⊢8),((3∘÷)⍣¯1⊢8),(÷∘3)⍣¯1⊢8" #(3/8 3/8 24))
  (for "Inverse exponents."      "(3⋆⍣¯1⊢8),((3∘⋆)⍣¯1⊢8),(⋆∘3)⍣¯1⊢8" #(1.8927892 1.8927892 2.0))
  (for "Inverse logarithms."     "(3⍟⍣¯1⊢8),((3∘⍟)⍣¯1⊢8),(⍟∘3)⍣¯1⊢8" #(6561 6561 1.1472027))
  (for "Inverse circular ops."   "y←⍳12 ⋄ (5○⍨-y)=(y∘○)⍣¯1⊢5" #(1 1 1 1 1 1 1 1 1 1 1 1))
  (for "Inverse indexing." "⍳⍣¯1⊢1 2 3 4 5" 5)
  (for "Inverse mix."      "↑⍣¯2⊢2 3 4⍴⍳9" #(#(#(1 2 3 4) #(5 6 7 8) #(9 1 2 3))
  					     #(#(4 5 6 7) #(8 9 1 2) #(3 4 5 6))))
  (for "Inverse split."    "↓⍣¯1⊢(1 2 3) (4 5 6) (7 8 9)" #2A((1 2 3) (4 5 6) (7 8 9)))
  (for "Inverse nest."     "⊆⍣¯1⊢⍳5" #(1 2 3 4 5))
  (for "Inverse disclose." "⊃⍣¯1⊢⍳5" #0A#(1 2 3 4 5))
  (for "Inverse reversal." "⌽⍣¯1⊢⍳5" #(1 2 3 4 5))
  (for "Inverse rotation." "(2⌽⍣¯1⊢⍳5)⍪1⊖⍣¯1⊢3 5⍴⍳9" #2A((4 5 1 2 3) (2 3 4 5 6) (1 2 3 4 5) (6 7 8 9 1)))
  (for "Inverse encode."   "1760 3 12⊤⍣¯1⊢2 0 10" 82)
  (for "Inverse decode."   "1760 3 12⊥⍣¯1⊢82" #(2 0 10))
  (for "Inverse composed decode extending left argument." "(2∘⊥)⍣¯1⊢5" #(1 0 1))
  (for "Inverse composed decode extending left argument with array as right argument."
       "(6∘⊥)⍣¯1⊢10 5 8 3" #2A((1 0 1 0) (4 5 2 3)))
  (for "Inversion of nested compound functions." "(3 +∘(2∘-)⍣¯1⊢5),3 +∘(2∘-)⍣¯1⊢5" #(0 0))
  (for "Inversion of Celsius-Fahrenheit conversion." "⌊(32∘+)∘(×∘1.8)⍣¯1⊢212" 100)
  (for "Inverse composed function." "(3(+∘÷∘-)⍣1⊢5),3(+∘÷∘-)⍣¯1⊢5" #(14/5 -1/2))
  (for "Inversion of scanning addition." "+\\⍣¯1⊢+\\⍳5" #(1 2 3 4 5))
  (for "Inversion of composed addition applied over each." "(+∘5)¨⍣¯1⊢-\\⍳5" #(-4 -6 -3 -7 -2))
  (for "Inversion of composed division applied over each." "(÷∘5)¨⍣¯1⊢+\\⍳5" #(5 15 30 50 75))
  (for "Double inversion of addition." "3 (+⍣¯1)⍣¯1⊢5" 8)
  (for "Commutative inversion of addition."       "+⍨⍣¯1⊢64" 32)
  (for "Commutative inversion of multiplication." "×⍨⍣¯1⊢64" 8.0)
  (for "Commutative inversion of max and min."    "(⌈⍨⍣¯1⊢64),⌊⍨⍣¯1⊢64" #(64 64))
  ;; (for "Inversion of commuted outer product." "((∘.×)∘4 5 6)⍣¯1⊢1 2 3∘.×4 5 6" #(1 2 3))
  ;; (for "Inversion of commuted outer product, other side." "(1 2 3∘(∘.×))⍣¯1⊢1 2 3∘.×4 5 6" #(4 5 6))
  ;; (for "More complex outer product inversion."
  ;;      "((∘.×)∘4 5 6)⍣¯1⊢((∘.×)∘4 5 6) (1 2 3∘(∘.+)) 10 20 30" #2A((11 21 31) (12 22 32) (13 23 33)))
  (for "Power set." "{⌿∘⍵¨↓⌽⍉2⊥⍣¯1⊢¯1+⍳2*≢⍵}'ab'" #("" "a" "b" "ab"))
  (for "Longer power set." "{⌿∘⍵¨↓⌽⍉2⊥⍣¯1⊢¯1+⍳2*≢⍵}'abc'"
       #("" "a" "b" "ab" "c" "ac" "bc" "abc"))
  (for "Inversion of variable-referenced function." "g←(3∘×) ⋄ g⍣¯1⊢24" 8)
  (for "Inversion of arbitrary function." "({3-⍵}⍣¯1⊢8),{⍵-3}⍣¯1⊢8" #(-5 11))
  (for "Inversion of more complex arbitrary function." "{5×2+⍵}⍣¯1⊢20" 2)
  (for "Even more complex function inverted." "{2*1+7-⍵}⍣¯1⊢64" 2.0)
  (for "Dyadic arbitrary function inverted." "(3 {⍵+÷-⍺}⍣¯1⊢5), 3 {⍺+÷-⍵}⍣¯1⊢5" #(1/2 -1/2)))
 
 (test-set
  (with (:name :printed-format-tests)
	(:tests-profile :title "Printed Data Format Tests")
	(:demo-profile :title "Data Format Demos"
		       :description "More demos showing how different types of data are formatted in April."))
  (for-printed "Single integer." "5" "5")
  (for-printed "Negative integer." "¯5" "¯5")
  (for-printed "Rational numbers." "÷⍳5" "1 1r2 1r3 1r4 1r5
")
  (for-printed "Floating point number." "25.006" "25.006")
  (for-printed "Imaginary number." "3J9" "3J9")
  (for-printed "Numeric vector." "1+1 2 3" "2 3 4
")
  (for-printed "Vector of mixed integers and floats." "12.5 3 42.890 90.5001 8 65"
	       "12.5 3 42.89 90.5001 8 65
")
  (for-printed "Oversized take of float vector, the filler zeroes printed without decimal points."
	       "6↑○⍳3" "3.141592654 6.283185307 9.424777961 0 0 0
")
  (for-printed "Numeric matrix." "3 4⍴⍳9" "1 2 3 4
5 6 7 8
9 1 2 3
")
  (for-printed "3D numeric array." "2 3 4⍴⍳9" "1 2 3 4
5 6 7 8
9 1 2 3
       
4 5 6 7
8 9 1 2
3 4 5 6
")
  (for-printed "4D numeric array." "2 3 2 5⍴⍳9" "1 2 3 4 5
6 7 8 9 1
         
2 3 4 5 6
7 8 9 1 2
         
3 4 5 6 7
8 9 1 2 3
         
         
4 5 6 7 8
9 1 2 3 4
         
5 6 7 8 9
1 2 3 4 5
         
6 7 8 9 1
2 3 4 5 6
")
  
  (for-printed "Vector of numeric matrices." "⊂[1 2]2 3 4⍴4 5 6"
	       " 4 5 6  5 6 4  6 4 5  4 5 6
 4 5 6  5 6 4  6 4 5  4 5 6
")
  (for-printed "Matrix of numeric matrices." "2 3⍴⊂2 2⍴⍳4"
	       " 1 2  1 2  1 2
 3 4  3 4  3 4
 1 2  1 2  1 2
 3 4  3 4  3 4
")
  (for-printed "Vector with nested vectors." "1 2 (1 2 3) 4 5 (6 7 8)"
	       "1 2  1 2 3  4 5  6 7 8
")
  (for-printed "Vector with initial nested vector." "(1 2 3) 4 5 (6 7) 8 9"
	       " 1 2 3  4 5  6 7  8 9
")
  (for-printed "Vector with nested arrays." "1 2 (3 4⍴⍳9) 5 6 (2 2 3⍴5) 7 8"
	       "1 2  1 2 3 4  5 6  5 5 5  7 8
     5 6 7 8       5 5 5     
     9 1 2 3                 
                   5 5 5     
                   5 5 5     
")
  (for-printed "Single scalar character." "'A'" "A")
  (for-printed "Character vector (string)." "'ABCDE'" "ABCDE")
  (for-printed "Character matrix." "2 5⍴'ABCDEFGHIJ'" "ABCDE
FGHIJ
")
  (for-printed "3D character array." "2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'" "GRAY
GOLD
BLUE
    
SILK
WOOL
YARN
")
  (for-printed "2D array of character strings." "⊂[3]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
	       " GRAY  GOLD  BLUE
 SILK  WOOL  YARN
")
  (for-printed "Vector of character matrices." "⊂[1 2]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
	       " GGB  ROL  ALU  YDE
 SWY  IOA  LOR  KLN
")
  (for-printed "Matrix of character matrices." "⊂[1 2]2 3 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
	       " GSG  RIR  ALA  YKY
 SGS  IRI  LAL  KYK
 GWG  OOO  LOL  DLD
 WGW  OOO  OLO  LDL
 BYB  LAL  URU  ENE
 YBY  ALA  RUR  NEN
")
  (for-printed "Stacked strings." "⍪'A' 'Stack' 'Of' 'Strings'"
	       " A      
 Stack  
 Of     
 Strings
")
  (for-printed "Mixed strings." "↑'These' 'Strings' 'Are' 'Mixed'"
	       "These  
Strings
Are    
Mixed  
")
  (for-printed "Enclosed vector." "⊂1 2 3 4 5" " 1 2 3 4 5
")
  (for-printed "Enclosed matrix." "⊂3 4⍴⍳9" " 1 2 3 4
 5 6 7 8
 9 1 2 3
")
  (for-printed "Matrix containing nested arrays of differing shapes." "{⊂⍺ ⍵}⌺3 3⊢3 3⍴⍳9"
	       "  1 1  0 0 0    1 0  0 0 0    1 ¯1  0 0 0 
       0 1 2         1 2 3          2 3 0 
       0 4 5         4 5 6          5 6 0 
  0 1  0 1 2    0 0  1 2 3    0 ¯1  2 3 0 
       0 4 5         4 5 6          5 6 0 
       0 7 8         7 8 9          8 9 0 
  ¯1 1  0 4 5   ¯1 0  4 5 6   ¯1 ¯1  5 6 0
        0 7 8         7 8 9          8 9 0
        0 0 0         0 0 0          0 0 0
")
  (for-printed "Array of differently-shaped nested arrays." "{⍺ ⍵}⌺3 3⊢3 3⍴⍳9"
	       " 1 1    0 0 0
        0 1 2
        0 4 5
 1 0    0 0 0
        1 2 3
        4 5 6
 1 ¯1   0 0 0
        2 3 0
        5 6 0
             
 0 1    0 1 2
        0 4 5
        0 7 8
 0 0    1 2 3
        4 5 6
        7 8 9
 0 ¯1   2 3 0
        5 6 0
        8 9 0
             
 ¯1 1   0 4 5
        0 7 8
        0 0 0
 ¯1 0   4 5 6
        7 8 9
        0 0 0
 ¯1 ¯1  5 6 0
        8 9 0
        0 0 0
")
  (for-printed "Nested vector with mixed numeric and character values."
	       "(1 2 'gh' 3) 4 'abc' (6 7) 8 9" " 1 2  gh  3  4  abc  6 7  8 9
")
  (for-printed "Column of integer and float values at varying precisions."
	       "⎕pp←6 ⋄ ⍪8 900.17814 3005 ¯15.90 88.1,÷2.0 4.0 8.0"
	       "   8    
 900.178
3005    
 ¯15.9  
  88.1  
   0.5  
   0.25 
   0.125
")
  (for-printed "Matrix of mixed strings and numeric vectors."
	       "2 2⍴'Test' (1 2 3) 'Hello' 5"
	       " Test   1 2 3
 Hello      5
")
  (for-printed "Matrix with columns of mixed string and numeric values."
	       "3 3⍴'a' 12 34 'b' 'cde' 'fgh' 'i' 900 'kl'"
	       "a  12   34
b cde  fgh
i 900   kl
")
  (for-printed "Another mixed matrix." "3 3⍴'a' 12 34 'b' 'cde' 'fgh' 'i' 900 'k'"
	       "a  12   34
b cde  fgh
i 900    k
")
  (for-printed "Another mixed matrix." "1⌽3 3⍴'a' 12 34 'b' 'cde' 'fgh' 'i' 900 'k'"
	       "  12   34 a
 cde  fgh b
 900    k i
")
  (for-printed "Another mixed matrix." "g←⍪12 'abc' 900 ⋄ g,(⍪1 2 3),g"
	       "  12  1   12
 abc  2  abc
 900  3  900
")
  (for-printed "Mixed matrix with floats." "1⌽3 3⍴'a' 12 3.045 'b' 'cde' 8.559 'i' 900 'k'"
	       "  12  3.045 a
 cde  8.559 b
 900      k i
")
  (for-printed "Mixed numeric, string and float matrix." "(⍪'abc'),(⍪1.2×1 2 2),⍪⍳3"
	       "a 1.2 1
b 2.4 2
c 2.4 3
")
  (for-printed "Matrix with intermixed character and float column." "(⍪'abc'),(⍪'a',1.2×1 2),⍪⍳3"
	       "a   a 1
b 1.2 2
c 2.4 3
")
  (for-printed "Catenated character and numeric arrays."
	       "(⍪⍳3),(3 3⍴'abcdef'),(3 3⍴⍳9),(3 3⍴'defghi'),⍪⍳3"
	       "1 abc 1 2 3 def 1
2 def 4 5 6 ghi 2
3 abc 7 8 9 def 3
")
  (for-printed "Array with mixed string/float column, string longer than floats."
	       "(⍪'abc'),(⍪(⊂'abcdef'),1.2 2.56),⍪⍳3"
	       "a abcdef  1
b   1.2   2
c   2.56  3
")
  (for-printed "Mixed array with nested multidimensional array."
	       "1⌽3 3⍴'a' 12 (2 4⍴5) 'b' 'cde' 'gg' 'i' 900 'k'"
	       "  12  5 5 5 5 a
      5 5 5 5  
 cde  gg      b
 900  k       i
")
  (for-printed "Another mixed/nested array."
	       "1⌽3 3⍴8 12 (2 4⍴5) 9 'cde' 'gg' 10 900 'k'"
	       "  12  5 5 5 5   8
      5 5 5 5    
 cde  gg        9
 900  k        10
")
  (for-printed "Mixed array with column holding longer number than nested array."
	       "(⍪22,2⍴⊂'abc'),(⍪(⊂2 2⍴1),'c' 12345678),⍪⍳3"
	       "  22       1 1  1
           1 1   
 abc         c  2
 abc  12345678  3
")
  (for-printed "Nested vector of vectors." "↓⍣2⊢2 2⍴⍳4" "  1 2  3 4
")
  (for-printed "Double-nested vector of vectors." "↓⍣3⊢2 2⍴⍳4" "   1 2  3 4
")
  (for-printed "Stacked floats with negative value under 1." "⍪¯0.75 1.25" "¯0.75
 1.25
")
  (for-printed "Stacked rational fractions and integers."
	       "⍪21r8 90 122r17 3r21 3 9r212"
	       " 21r__8
 90    
122r_17
  1r__7
  3    
  9r212
")
  (for-printed "Complex rational vector." "3r4J9r5×⍳4"
	       "3r4J9r5 3r2J18r5 9r4J27r5 3J36r5
")
  (for-printed "Matrix of complex numbers." "3 3⍴3.2J5.3 32.95J12.15"
	       " 3.20J_5.3  32.95J12.15  3.20J_5.3 
32.95J12.15  3.20J_5.3  32.95J12.15
 3.20J_5.3  32.95J12.15  3.20J_5.3 
")
  (for-printed "Another complex matrix." "⎕pp←10 ⋄ ¯2 ¯3 ¯4 ¯5 ¯6 ∘.○ ¯2 ¯2J3r5"
	       " 3.141592654J¯1.316957897  2.813351012J¯1.379764132
¯1.107148718              ¯1.134728076J_0.113868882
¯1.732050808              ¯1.761796881J_0.681122786
¯1.443635475              ¯1.475251760J_0.263872484
 1.316957897J_3.141592654  1.379764132J_2.813351012
")
  (for-printed "Stacked complex float, integer and rational."
	       "⍪12.2J44 3J8 19J210r17"
	       "12.2J_44.0 
 3__J__8   
19__J210r17
")
  (for-printed "Stacked complex float, integer and non-complex rational."
	       "⍪12.2J44 3J8 19r13"
	       "12.20J44.0
 3___J_8  
19r13     
")
  (for-printed "Stacked complex float and rational fractions."
	       "⍪12.2J44 3r8J8r21 19r313J21r17"
	       "12.200J44.0 
 3r__8J_8r21
19r313J21r17
")
  (for-printed "Output of variable assignment (just a newline)." "x←1" "")
  (for-printed "Binomial of complex numbers." "⎕pp←4 ⋄ 2!3J2" "1.000J5.000")
  (for-printed "Binomial of positive and negative fractional numbers." "⎕pp←5 ⋄ 3!.05 2.5 ¯3.6"
	       "0.0154 0.3125 ¯15.456
")
  (for-printed "Function name." "⎕pp←10 ⋄ fun←{⍵+5} ⋄ fun" "∇fun"))
 
 (arbitrary-test-set
  (with (:name :output-specification-tests)
	(:tests-profile :title "Output Specification Tests"))
  ((progn (princ (format nil "λ Evaluation of ⍳ with specified index origin.~%"))
	  (is (print-and-run (april (with (:state :index-origin 0)) "⍳9"))
	      #(0 1 2 3 4 5 6 7 8) :test #'equalp))
   (let ((out-str (make-string-output-stream)))
     (princ (format nil "λ Printed output at given precisions.~%"))
     (print-and-run (april-f (with (:state :print-to out-str :print-precision 3)) "○1 2 3"))
     (is (get-output-stream-string out-str)
	 "3.14 6.28 9.42
")
     (princ (format nil "~%"))
     (print-and-run (april-f (with (:state :print-to out-str :print-precision 6)) "○1 2 3"))
     (is (get-output-stream-string out-str)
	 "3.14159 6.28319 9.42478
")

     (princ (format nil "~%λ Output of function definition (just a newline).~%"))
     (print-and-run (april-f (with (:state :print-to out-str)) "{⍵+3}"))
     (is (get-output-stream-string out-str)
	 "
"))
   (progn (princ (format nil "λ Floating-point comparisons with varying comparison tolerance.~%"))
	  (is (print-and-run (april-c "{G←1.00001<1.0001 ⋄ ⎕ct←0.0001 ⋄ H←G,1.00001<1.0001 ⋄ ⎕ct←⍵ ⋄ H}"
				      double-float-epsilon))
	      #*10))
   (progn (princ (format nil "λ Output of one input and one declared variable with index origin set to 0.~%"))
	  (multiple-value-bind (out1 out2)
	      (print-and-run (april (with (:state :count-from 0 :in ((a 3) (b 5))
						  :out (a c)))
				    "c←a+⍳b"))
	    (is out1 3)
	    (princ (format nil "~%"))
	    (is out2 #(3 4 5 6 7) :test #'equalp)))
   (progn (princ (format nil "λ Output of both value and APL-formatted value string.~%"))
	  (multiple-value-bind (out1 out2)
	      (print-and-run (april (with (:state :output-printed t)) "2 3⍴⍳9"))
	    
	    (is out1 #2A((1 2 3) (4 5 6)) :test #'equalp)
	    (princ (format nil "~%"))
	    (is out2 "1 2 3
4 5 6
")))
   (progn (format nil "λ Output of APL-formatted value string alone.~%")
	  (is (print-and-run (april (with (:state :output-printed :only)) "2 3⍴⍳9"))
	      "1 2 3
4 5 6
"))
   (progn (princ (format nil "λ Output of three internally-declared variables.~%"))
	  (multiple-value-bind (out1 out2 out3)
	      (print-and-run (april (with (:state :out (a b c)))
				    "a←9+2 ⋄ b←5+3 ⋄ c←2×9"))
	    (princ (format nil "~%"))
	    
	    (is out1 11)
	    (princ (format nil "~%"))
	    (is out2 8)
	    (princ (format nil "~%"))
	    (is out3 18)))
   (progn (princ (format nil "λ Output using ⎕← to specified output stream.~%"))
	  (let* ((out-str (make-string-output-stream))
		 (vector (print-and-run (april (with (:state :print-to out-str))
					       "a←1 2 3 ⋄ ⎕←a+5 ⋄ ⎕←3 4 5 ⋄ 3+a"))))

	    (is vector #(4 5 6) :test #'equalp)

	    (princ (format nil "~%"))
	    
	    (is (print-and-run (get-output-stream-string out-str))
		"6 7 8
3 4 5
")))
   (progn (princ (format nil "λ Printed output of a variable assignment preceded by ⎕←.~%"))
	  (let* ((out-str (make-string-output-stream))
		 (vector (print-and-run (april (with (:state :print-to out-str))
					       "⎕←x←1 2 3"))))

	    (is vector #(1 2 3) :test #'equalp)

	    (princ (format nil "~%"))
	    
	    (is (print-and-run (get-output-stream-string out-str))
		"1 2 3
")))
   (let* ((out-str (make-string-output-stream))
	  (other-out-str (make-string-output-stream)))
     (print-and-run (april-f "a←1 2 3 ⋄ ⎕ost←('APRIL' 'OUT-STR') ⋄ ⎕←a+5 ⋄ ⎕←3 4 5 
⎕ost←('APRIL' 'OTHER-OUT-STR') ⋄ 3+a"))
     (princ (format nil "~%~%"))
     (is (print-and-run (get-output-stream-string out-str))
	 "6 7 8
3 4 5
" :test #'equalp)
     (princ (format nil "~%"))
     (is (print-and-run (get-output-stream-string other-out-str))
	 "4 5 6
" :test #'equalp))
   (progn (princ (format nil "λ Multi-line function with comment at end.~%"))
	  
	  (is (print-and-run (april "fun←{
 5+⍵
 ⍝ comment
}
fun 3")) 8))
   (progn (princ (format nil "λ Compact function calls.~%"))
	  
	  (is (print-and-run (april-c "{⍺×⍵}" 2 8)) 16)
	  
	  (princ (format nil "~%"))
	  
	  (is (print-and-run (april-c "{[a;b;c;d] d↑c⍴a+b}" 3 5 6 10))
	      #(8 8 8 8 8 8 0 0 0 0) :test #'equalp)
	  
	  (princ (format nil "~%"))

	  (is (print-and-run (april-c (with (:state :count-from 0)) "{⍳⍵}" 7))
	      #(0 1 2 3 4 5 6) :test #'equalp))
   
   )))

(april-create-workspace common)
(april-create-workspace unit-test-staging)

#|
This is an example showing how the April idiom can be extended with Vex's extend-vex-idiom macro.
A not-very-useful scalar function that adds 3 to its argument(s) is specified here.

(extend-vex-idiom
 april
 (utilities :process-lexicon #'april-function-glyph-processor)
 (functions
  (with (:name :extra-functions))
  (⍛ (has :title "Add3")
     (ambivalent (scalar-function (λω (+ 3 omega)))
		 (scalar-function (lambda (alpha omega) (+ 3 alpha omega))))
     (tests (is "⍛77" 80)
	    (is "8⍛7" 18)))))
|#

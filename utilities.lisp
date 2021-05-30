;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; utilities.lisp

(in-package #:april)

"Utility functions for April. It's important to understand the difference between the functions and macros provided here and the ones that come from the aplesque package. The faculties provided by aplesque reflect features of APL, but they have uses other than implementing APL. The material here is specifically for use in implementing APL, with uses aside from an APL implementation not accounted for in its design. The functions here are used to implement language mechanics as opposed to functions in the language's standard library; the latter are implemented in library.lisp."

(define-symbol-macro this-idiom *april-idiom*)
(define-symbol-macro *apl-timestamp* (apl-timestamp))
(define-symbol-macro *first-axis* (if (not axes) 0 (- (first axes) index-origin)))
(define-symbol-macro *last-axis* (if axes (- (first axes) index-origin)
				     (max 0 (1- (rank omega)))))
(define-symbol-macro *first-axis-or-nil* (if axes (- (first axes) index-origin)))
(define-symbol-macro *branches* (symbol-value (intern "*BRANCHES*" space)))

(defvar *function-identities* nil)

;; the names of library functions that curry functions having axes with index-origin, needed for the λχ macro
(defparameter *io-currying-function-symbols-monadic* '(ravel-arrays))
(defparameter *io-currying-function-symbols-dyadic* '(catenate-arrays catenate-on-first section-array))
(defparameter *package-name-string* (package-name *package*))

(defvar *april-parallel-kernel*)

(defun make-threading-kernel-if-absent ()
  (if (not lparallel:*kernel*)
      (setq lparallel:*kernel* (setq *april-parallel-kernel*
				     (lparallel:make-kernel (1- (cl-cpus:get-number-of-processors))
							    :name "april-language-kernel")))))

(let ((this-package (package-name *package*)))
  (defmacro in-april-workspace (name &body body)
    "Reader macro that interns symbols in the current workspace; works in tandem with 𝕊 reader macro."
    (let ((space-name (concatenate 'string "APRIL-WORKSPACE-" (string-upcase name))))
      (labels ((replace-symbols (form)
		 (loop :for item :in form :for ix :from 0
		    :do (if (listp item)
			    (if (and (second item) (not (third item))
				     (symbolp (second item)) (eql 'inws (first item)))
				(setf (nth ix form) (intern (string (second item)) space-name))
				(replace-symbols item))
			    (if (and (symbolp item)
				     (string= "+WORKSPACE-NAME+" (string-upcase item)))
				(setf (nth ix form) (intern (string-upcase name)
							    this-package)))))))
	(replace-symbols body)
	(setf (cdddr (first body))
	      (cons '(make-threading-kernel-if-absent)
		    (cdddr (first body))))
	(first body)))))

;; this reader macro expands to (inws symbol) for reader-friendly printing of compiled code
(set-macro-character #\𝕊 (lambda (stream character)
			   (declare (ignore character))
			   (list 'inws (read stream t nil t))))

;; printer extension to use the 𝕊 reader macro
(set-pprint-dispatch '(cons (member inws)) 
		     #'(lambda (s list)
			 (if (and (symbolp (second list)) (not (third list)))
			     (funcall (formatter "𝕊~W") s (second list)) 
			     (pprint-fill s list))))

(defun disclose-atom (item)
  "If the argument is a non-nested array with only one member, disclose it, otherwise do nothing."
  (if (not (and (not (stringp item)) (arrayp item) (is-unitary item)
		(not (arrayp (row-major-aref item 0)))))
      item (row-major-aref item 0)))

(defmacro insym (symbol)
  "Macro used in grammar.lisp to intern value-referencing symbols in appropriate workspace package."
  `(if (or (not (symbolp ,symbol))
	   (member ,symbol '(⍵ ⍺))) ;; TODO: this is a hack for now, create a list of all reserved symbol names
       ,symbol (intern (string ,symbol) space)))

(defmacro alambda (params &body body)
  `(labels ((∇self ,params ,@body))
     #'∇self))

(defmacro olambda (params &body body)
  `(labels ((∇oself ,params ,@body))
     #'∇oself))

(defmacro achoose (item indices &rest rest-params)
  (let ((indices-evaluated (gensym)))
    `(let ((,indices-evaluated ,indices))
       (choose ,item ,indices-evaluated ,@rest-params))))

(defun dummy-nargument-function (first &rest rest)
  "Placeholder function to be assigned to newly initialized function symbols."
  (declare (ignorable rest))
  first)

;; keep legacy april-p macro in place and usable in place of april-f
(defmacro april-p (&rest args)
  (cons 'april-f args))

;; these macros are shorthand for lambda definitions used in the spec; they make April's compiled code
;; more compact and comfortable to read
(defmacro λω (&rest body)
  `(lambda (omega) ,@body))

(defmacro λωα (&rest body)
  `(lambda (omega alpha) ,@body))

(defmacro λωαχ (&rest body)
  `(lambda (omega alpha &optional axes) ,@body))

(defmacro λωχ (&rest body)
  `(lambda (omega &optional axes) ,@body))

(defmacro λχ (body axes)
  "Curry a function with axes for use with an operator."
  (if (member (first body) (cons 'λωαχ *io-currying-function-symbols-dyadic*))
      `(λωα (funcall ,body omega alpha ,(cons 'list axes)))
      (if (member (first body) (cons 'λωχ *io-currying-function-symbols-monadic*))
	  `(λω (funcall ,body omega ,(cons 'list axes)))
	  body)))

(defmacro is-workspace-value (item)
  "Checks if a variable is present in the current workspace as a value."
  `(and (boundp (intern (string ,item) space))
	(not (fboundp (intern (string ,item) space)))))

(defmacro is-workspace-function (item)
  "Checks if a variable is present in the current workspace as a function."
  `(fboundp (intern (string ,item) space)))

(defmacro is-workspace-operator (item)
  "Checks if a variable is present in the current workspace as a function."
  `(or (fboundp (intern (concatenate 'string "𝕆𝕃∇" (string ,item))
			space))
       (fboundp (intern (concatenate 'string "𝕆ℙ∇" (string ,item))
			space))))

(defun set-workspace-item-meta (symbol item &rest data)
  "Sets one or more metadata for an item in a workspace."
  (if (not (boundp symbol))
      (set symbol (make-hash-table :test #'eq)))
  (loop :for (key value) :on data :by #'cddr
     :do (setf (getf (gethash item (symbol-value symbol)) key) value)))

(defun get-workspace-item-meta (symbol item &rest keys)
  "Gets one or more metadata for an item in a workspace."
  (if (boundp symbol)
      (let ((data (gethash item (symbol-value symbol))))
	(apply #'values (loop :for key :in keys :collect (getf data key))))))

(defun get-workspace-alias (space symbol)
  "Find an existing alias of a lexical function in a workspace."
  (let ((aliases-symbol (intern "*LEXICAL-FUNCTION-ALIASES*" space))
	(ws-symbol (intern (string symbol) space)))
    (if (boundp aliases-symbol) (gethash ws-symbol (symbol-value aliases-symbol)))))

(defun set-workspace-alias (space symbol glyph)
  "Set an alias for a lexical function in a workspace, as when compiling f←+."
  (let ((aliases-symbol (intern "*LEXICAL-FUNCTION-ALIASES*" space))
	(ws-symbol (intern (string symbol) space)))
    (if (not (boundp aliases-symbol))
	(setf (symbol-value aliases-symbol) (make-hash-table :test #'eq)))
    (let ((ws-aliases (symbol-value aliases-symbol)))
      (if (or (and (null glyph)
		   (gethash ws-symbol ws-aliases))
	      (resolve-function :monadic glyph)
	      (resolve-function :dyadic glyph))
	  (setf (gethash ws-symbol ws-aliases) glyph)))))
  
(defun build-populator (metadata-symbol input)
  "Generate a function that will populate array elements with an empty array prototype." 
  (if (and (= 0 (size input))
	   (get-workspace-item-meta metadata-symbol input :eaprototype))
      (lambda () (copy-nested-array (get-workspace-item-meta metadata-symbol input :eaprototype)))))

(defun make-prototype-of (array)
  "Make a prototype version of an array; all values in the array will be blank spaces for character arrays or zeroes for other types of arrays."
  (if (not (eq t (element-type array)))
      (make-array (dims array) :element-type (element-type array)
		  :initial-element (if (member (element-type array) '(base-char character)) #\  0))
      (let ((output (make-array (dims array))))
	(dotimes (i (size output)) (setf (row-major-aref output i)
					 (if (not (arrayp (row-major-aref array i)))
					     0 (make-prototype-of (row-major-aref array i)))))
	output)))

(defmacro print-and-run (form)
  "Print a formatted code string and then run the code; used in april's arbitrary evaluation tests."
  `(let ((*print-case* :downcase))
     (princ (indent-code (write-to-string (quote ,form))))
     ,form))

(defun indent-code (string)
  "Indent a code string produced by (print-and-run) as appropriate for April's test output."
  (concatenate 'string "  * " (regex-replace-all "[\\n]" string (format nil "~%    "))))

(defmacro apl-assign (symbol value)
  "This is macro is used to build variable assignment forms and includes logic for stranded assignments."
  (if (or (not (listp symbol))
	  (eql 'inws (first symbol)))
      `(setq ,symbol ,(if (not (or (symbolp value)
				   (and (listp value)
					(eql 'inws (first value)))))
			  value `(duplicate ,value)))
      (let ((assign-forms) (values (gensym "A"))
	    (symbols (if (not (eql 'avector (first symbol)))
			 symbol (rest symbol))))
	(labels ((build-aref (symbol path)
		   (if (not path) symbol (build-aref `(aref ,symbol ,(first path)) (rest path))))
		 (process-symbols (sym-list &optional path)
		   (loop :for s :in sym-list :for sx :from 0
		      :do (let ((path-to (cons sx path)))
			    (if (and (listp s) (not (eql 'inws (first s))))
				(process-symbols s path-to)
				(let ((set-to `(disclose (if (or (not (arrayp ,values)) (= 1 (size ,values)))
							     ,values ,(build-aref values (reverse path-to))))))
				  (setq assign-forms (cons `(setq ,s ,set-to)
							   assign-forms))))))))
	  (process-symbols symbols)
	  `(let ((,values ,value))
	   (if (and (arrayp ,values)
		    (/= 1 (size ,values)) (/= ,(1- (length symbol)) (length ,values)))
	       (error "Mismatched number of symbols and values for string assignment."))
	   ,@assign-forms ,values)))))

(defmacro apl-output (form &key (print-to) (output-printed)
			     (print-assignment) (print-precision) (with-newline))
  "Generate code to output the result of APL evaluation, with options to print an APL-formatted text string expressing said result and/or return the text string as a result."
  (let ((result (gensym)) (printout (gensym))
	;; get the symbol referencing a function passed as the output
	(function-name-value (if (and (listp form) (eql 'function (first form)))
				 `(string (quote ,(second form))))))
    `(let* ((,result ,form)
	    (,printout ,(if (and (or print-to output-printed))
			    ;; don't print the results of assignment unless the :print-assignment option is set,
			    ;; as done when compiling a ⎕← expression
			    (or (and function-name-value
				     `(concatenate 'string "∇" ,function-name-value))
				;; if a bare function name is to be output, prefix it with ∇
				(and (listp form)
				     (eql 'apl-assign (first form))
				     (not print-assignment)
				     "")
				`(matrix-print ,result :append #\Newline
					       :segment (lambda (n &optional s)
							  (count-segments n ,print-precision s))
					       :format (lambda (n &optional s r)
							 (print-apl-number-string
							  n s ,print-precision nil r)))))))
       (declare (ignorable ,result ,printout))
       ;; TODO: add printing rules for functions like {⍵+1}
       ,(if print-to (let ((string-output `(progn (write-string ,printout ,print-to))))
		       `(progn (if (arrayp ,result)
				   ,string-output (concatenate 'string ,string-output (list #\Newline)))
			       ,@(if with-newline
				     `((if (not (char= #\Newline (aref ,printout (1- (size ,printout)))))
		       			   (write-char #\Newline ,print-to)))))))
       ,(if output-printed (if (eq :only output-printed) printout `(values ,result ,printout))
	    result))))

(defun array-to-nested-vector (array)
  "Convert an array to a nested vector. Useful for applications such as JSON conversion where multidimensional arrays must be converted to nested vectors."
  (aops:each (lambda (member) (if (not (and (arrayp member) (< 1 (rank member))))
				  member (array-to-nested-vector member)))
	     (aops:split array 1)))

(defmacro avector (&rest items)
  "This macro returns an APL vector, disclosing data within that are meant to be individual atoms."
  (let ((type))
    (loop :for item :in items :while (not (eq t type))
       :do (setq type (type-in-common type (assign-element-type (if (or (not (integerp item))
									(> 0 item))
								    item (max 16 item))))))
    `(make-array (list ,(length items))
		 :element-type (quote ,type)
		 ;; enclose each array included in an APL vector
		 :initial-contents (list ,@items))))

(defun numeric-string-p (string)
  "Checks whether the argument is a numeric string."
  (ignore-errors (parse-apl-number-string string)))

(defun parse-apl-number-string (number-string &optional component-of)
  "Parse an APL numeric string into a Lisp value, handling high minus signs, J-notation for complex numbers and R-notation for rational numbers."
  (let ((nstring (string-upcase (regex-replace-all "[_]" number-string ""))))
    (if (and (not (eql 'complex component-of))
	     (find #\J nstring))
	(let ((halves (cl-ppcre:split #\J nstring)))
	  (if (and (= 2 (length halves))
		   (< 0 (length (first halves)))
		   (< 0 (length (second halves))))
	      (complex (parse-apl-number-string (first halves) 'complex)
		       (parse-apl-number-string (second halves) 'complex))))
	(if (find #\E nstring)
	    (let ((exp-float (parse-number:parse-number (regex-replace-all "[¯]" nstring "-")
							:float-format 'double-float)))
	      (if (< double-float-epsilon (nth-value 1 (floor exp-float)))
		  exp-float (let ((halves (mapcar #'parse-apl-number-string (cl-ppcre:split #\E nstring))))
			      (floor (* (first halves) (expt 10 (second halves)))))))
	    (if (and (not (eql 'rational component-of))
		     (find #\R nstring))
		(let ((halves (cl-ppcre:split #\R nstring)))
		  (/ (parse-apl-number-string (first halves) 'rational)
		     (parse-apl-number-string (second halves) 'rational)))
		;; the macron character is converted to the minus sign
		(parse-number:parse-number (regex-replace-all "[¯]" nstring "-")
					   :float-format 'double-float))))))

(defun print-apl-number-string (number &optional segments precision decimals realpart-multisegment)
  "Format a number as appropriate for APL, using high minus signs and J-notation for complex numbers, optionally at a given precision and post-decimal length for floats."
  (cond ((complexp number)
	 (format nil "~aJ~a" (print-apl-number-string (realpart number)
						      (list (first segments)
							    (if (or realpart-multisegment
								    (not (integerp (realpart number))))
								(if (not (third segments))
								    0 (- (second segments)))))
						      precision nil t)
		 (print-apl-number-string (imagpart number) (if (third segments)
								(list (- (third segments))
								      (or (fourth segments) 0))
								(list (second segments) 0))
					  precision)))
	((integerp number)
	 (let ((output (format nil (format nil "~~~d,'~ad~a" (abs (first segments))
					   ;; for negative values, empty space to the left must initially
					   ;; be filled with ¯ characters; the reasoning is explained below
					   (if (> 0 number) #\¯ (if (> 0 (first segments)) #\_ #\ ))
					   (if (not (and (second segments)
							 (or (> 0 (first segments))
							     (> 0 (second segments)))))
					       "" (make-array (1+ (abs (second segments)))
							      :element-type 'base-char :initial-element
							      (if (and (< 0 (first segments))
								       (second segments)
								       (> 0 (second segments)))
								  #\_ #\ ))))
			       (abs number)))
	       (number-found))
	   (if (> 0 number)
	       ;; replace ¯ padding with zeroes or spaces as appropriate; this strange system
	       ;; of initially padding with ¯ is needed because ¯ is an extended unicode character
	       ;; and strings of numeric characters are rendered as base-char arrays by (format),
	       ;; making it impossible to assign ¯ to their elements; unicode characters must be generated
	       ;; by (format) so that the output string is of type 'character; this is also done for floats
	       (loop :for i :from 1 :to (1- (length output)) :while (not number-found)
		  :do (if (alphanumericp (aref output i))
			  (setq number-found t)
			  (setf (aref output (1- i)) #\ ))))
	   output))
	((rationalp number)
	 (format nil "~ar~a" (print-apl-number-string (numerator number) (list (first segments)) precision)
		 (print-apl-number-string (denominator number) (list (- (abs (second segments)))) precision)))
	(t (let* ((number-string (first (cl-ppcre:split #\D (string-upcase (write-to-string number)))))
		  (number-sections (cl-ppcre:split #\. number-string))
		  (right-padding (if (not (and (second segments) (< 0 (second segments))))
				     0 (max 0 (- (second segments) (length (second number-sections))))))
		  ;; space to left of decimal is expressed by the first segment
		  (left-space (abs (first segments)))
		  ;; space to right of decimal can be explicitly specified by decimal argument
		  ;; or expressed by second segment length, whose length may be expanded if the digits
		  ;; on the left don't fill out the precision, as with ⎕pp←6 ⋄ ⍪3005 0.125
		  (right-space (+ (or decimals (max 1 (- (max (abs (second segments))
							      (min (length (second number-sections))
								   (abs (second segments))))
							 right-padding)))))
		  ;; total number length is left space + right space + one for decimal dot
		  (total-length (+ 1 left-space right-space))
		  (output (format nil (format nil "~~~d,~d,,,'~af~a" total-length right-space
					      (if (> 0 number)
						  #\¯ (if (< 0 (first segments)) #\  #\_))
					      (if (not (and right-padding (< 0 right-padding)))
						  "" (make-array right-padding :element-type 'base-char
								 :initial-element #\ )))
				  (abs number))))
	     (if (> 0 number)
		 (let ((start-at (if (< 0 (first segments)) 0 1)))
		   (loop :for i :from start-at :while (char= #\¯ (aref output i))
		      :when (or (= 1 start-at) (char= #\¯ (aref output (1+ i))))
		      :do (setf (aref output i) (aref " 0" start-at)))))
	     output))))

(defun format-value (idiom-name symbols element)
  "Convert a token string into an APL value, paying heed to APL's native ⍺, ⍵ and ⍬ variables."
  (cond ((string= element "⍬")
	 ;; APL's "zilde" character yields a keyword the compiler translates to an empty vector
	 :empty-array)
	((or (and (char= #\" (aref element 0))
		  (char= #\" (aref element (1- (length element)))))
	     (and (char= #\' (aref element 0))
		  (char= #\' (aref element (1- (length element))))))
	 ;; strings are converted to Lisp strings and passed through,
	 ;; unless they're one element in which case the character is disclosed
	 (if (= 3 (length element))
	     (aref element 1) (subseq element 1 (1- (length element)))))
	((member element '("⍺" "⍵" "∇" "⍺⍺" "⍵⍵" "∇∇") :test #'string=)
	 ;; alpha and omega characters are directly changed to symbols in the April package
	 (values (intern element idiom-name) t))
	((numeric-string-p element)
	 (parse-apl-number-string element))
	(t (or (and (char= #\⎕ (aref element 0))
		    (or (getf (rest (assoc :variable symbols))
			      (intern (string-upcase element) "APRIL"))
			(getf (rest (assoc :constant symbols))
			      (intern (string-upcase element) "APRIL"))))
	       (values (intern element) t)))))

(defun apl-timestamp ()
  "Generate an APL timestamp, a vector of the current year, month, day, hour, minute, second and millisecond."
  (let ((now (now)))
    (make-array 7 :element-type '(integer 0 16384)
		:initial-contents (list (year-of now) (month-of now) (day-of now) (hour-of now)
					(minute-of now) (second-of now) (millisecond-of now)))))

(defun process-output-vector (items)
  "Process items in a vector to be generated by the compiler, wrapping any array references in aplSymbol so that they are disclosed. This does not apply if the output vector is unitary (length 1)."
  (loop :for item :in items :collect (if (and (< 1 (length items))
					      (listp item) (eql 'achoose (first item)))
					 (list 'disclose item)
					 item)))

(defmacro with-derived-operands (operand-specs &rest body)
  "Derive references to data to be passed to an operator so they can be used by the macro implementing the operator."
  (let* ((first-op (gensym)) (first-axes (gensym)) (second-op (gensym)) (second-axes (gensym))
	 (ignorables) (to-ignore))
    (loop :for symbol :in '(right left)
       :do (if (not (member symbol operand-specs)) (setq to-ignore (cons symbol to-ignore))))
    `(lambda (,first-op ,first-axes &optional ,second-op ,second-axes)
       (declare (ignorable ,second-op ,second-axes))
       (let (,@(loop :for symbol :in operand-specs
		:when (and (not (member symbol '(right left axes)))
			   (or (symbolp symbol) (characterp symbol)))
		:collect (list symbol (case symbol
					(left-op first-op)
					(left-axes first-axes)
					(left-glyph (setq ignorables (cons 'left-glyph ignorables))
						    `(or-functional-character ,first-op :fn))
					(left-fn-monadic
					 `(if (resolve-function :monadic ,first-op ,first-axes)
					      `(λω (apl-call ,(or-functional-character ,first-op :fn)
							     ,(resolve-function :monadic ,first-op ,first-axes)
							     omega))
					      (if (and (listp ,first-op) (eql 'function (first ,first-op)))
						  ,first-op (if (eql '⍺⍺ ,first-op) ,first-op))))
					(left-fn-monadic-inverse
					 `(if (resolve-function :monadic-inverse ,first-op ,first-axes)
					      `(λω (apl-call ,(or-functional-character ,first-op :fn)
							     ,(resolve-function :monadic-inverse
										,first-op ,first-axes)
							     omega))))
					(left-fn-dyadic
					 `(if (resolve-function :dyadic ,first-op ,first-axes)
					      `(λωα (apl-call ,(or-functional-character ,first-op :fn)
							      ,(resolve-function :dyadic ,first-op ,first-axes)
							      omega alpha))
					      (if (and (listp ,first-op) (eql 'function (first ,first-op)))
						  ,first-op (if (eql '⍺⍺ ,first-op) ,first-op))))
					(left-fn-dyadic-inverse
					 `(if (resolve-function :dyadic-inverse ,first-op)
					      `(λωα (apl-call ,(or-functional-character ,first-op :fn)
							      ,(getf
								(resolve-function :dyadic-inverse
										  ,first-op)
								:plain)
							      omega alpha))))
					(left-fn-symbolic `(resolve-function :symbolic ,first-op ,first-axes))
					(right-op second-op)
					(right-axes second-axes)
					(right-glyph (setq ignorables (cons 'right-glyph ignorables))
					 `(or-functional-character ,second-op :fn))
					(right-fn-monadic
					 `(if (resolve-function :monadic ,second-op ,second-axes)
					      `(λω (apl-call ,(or-functional-character ,second-op :fn)
							     ,(resolve-function :monadic
										,second-op ,second-axes)
							     omega))
					      (if (and (listp ,second-op) (eql 'function (first ,second-op)))
						  ,second-op (if (eql '⍵⍵ ,second-op) ,second-op))))
					(right-fn-dyadic
					 `(if (resolve-function :dyadic ,second-op ,second-axes)
					      `(λωα (apl-call ,(or-functional-character ,second-op :fn)
							      ,(resolve-function :dyadic ,second-op ,second-axes)
							      omega alpha))
					      (if (and (listp ,second-op) (eql 'function (first ,second-op)))
						  ,second-op (if (eql '⍵⍵ ,second-op) ,second-op))))
					(right-fn-symbolic `(resolve-function :symbolic
									      ,second-op ,second-axes))))))
	 ,@(if ignorables `((declare (ignorable ,@ignorables))))
	 (lambda (,@(if (member 'axes operand-specs)
			(list 'axes)
			(list 'right 'left)))
	   (declare ,@(if (member 'axes operand-specs) `((ignorable axes))
			  `((ignore ,@to-ignore))))
	   ;; if bare character values were passed in wrapped in (:char) forms,
	   ;; they are removed from those forms here
	   ,@(if (not (member 'axes operand-specs))
		 `((if (and (listp right) (eq :char (first right)))
		       (setq right (second right)))
		   (if (and (listp left) (eq :char (first left)))
		       (setq left (second left)))))
	   ,@body)))))

(defun resolve-function (mode reference &optional axes)
  "Retrieve the function corresponding to a given character or symbol in a given mode (monadic or dyadic)."
  (if (characterp reference)
      (if axes `(λχ ,(of-functions this-idiom reference mode) ,axes)
	  (of-functions this-idiom reference mode))
      (if (and (symbolp reference) (fboundp reference))
	  `(function ,reference)
	  ;; TODO: can the logic determining if something is not a function be improved?
	  (if (and (listp reference)
		   (or (eql 'lambda (first reference))
		       (and (symbolp (first reference))
			    (macro-function (first reference))
			    (not (member (first reference)
					 ;; TODO: this will cause a problem if a function is passed and assigned
					 '(avector apl-call apl-if apl-output apl-assign))))))
	      reference))))

(defmacro resolve-operator (mode reference)
  "Retrieve an operator's composing function."
  `(of-operators this-idiom ,reference ,mode))

(defun extract-axes (process tokens &optional axes)
  "Given a list of tokens starting with axis specifications, build the code for the axis specifications to be applied to the subsequent function or value."
  (labels ((process-axis (axis)
	     (multiple-value-bind (item item-props remaining)
		 (funcall process axis)
	       (declare (ignore remaining))
	       ;; allow either a null item (representing an elided axis) or an array
	       (if (or (not item) (eq :array (first (getf item-props :type))))
		   item (error "Invalid axis.")))))
    (if (and (listp (first tokens))
	     (eq :axes (caar tokens)))
	(extract-axes process (rest tokens)
		      (cons (loop :for axis :in (cdar tokens)
			       :collect (if (= 1 (length axis))
					    (process-axis axis)
					    (cons 'progn (mapcar #'process-axis axis))))
			    axes))
	(values axes (first tokens)
		(rest tokens)))))

(defun adjust-axes-for-index-origin (io axis-list)
  "Adjust axes passed to a function to account for the given index origin."
  (if (integerp (first axis-list))
      (- (first axis-list) io)
      (if (vectorp (first axis-list))
	  (let ((ix 0)
		(output (make-array (list (length (first axis-list))))))
	    (loop :for i :across (first axis-list) :do (setf (aref output ix) (- i io)
							     ix (1+ ix)))
	    output))))

(defmacro apl-call (symbol function &rest arguments)
  "Call an APL function with one or two arguments. Compose successive scalar functions into bigger functions for more efficiency."
  (declare (ignore symbol))
  (let ((arg (gensym "A")))
    (flet ((is-scalar (form) (and (listp form) (eql 'scalar-function (first form))))
	   (is-boolean (form) (and (listp form) (listp (second form))
				   (eql 'boolean-op (caadr form))))
	   (expand-monadic (fn argument)
	     (let ((arg-expanded (macroexpand argument)))
	       (if (and (listp arg-expanded)
			(eql 'apply-scalar (first arg-expanded))
			(not (fourth arg-expanded)))
		   (let ((innerfn (second arg-expanded)))
		     (list (if (not (eql 'lambda (first innerfn)))
			       `(lambda (,arg) (funcall ,fn (funcall ,innerfn ,arg)))
			       (list (first innerfn) (second innerfn)
				     `(funcall ,fn ,(third innerfn))))
			   (third arg-expanded)))
		   (list fn argument))))
	   (expand-dyadic (fn is-first arg1 arg2)
	     (let* ((arg-expanded (macroexpand (if is-first arg1 arg2))))
	       (if (and (listp arg-expanded)
			(eql 'apply-scalar (first arg-expanded))
			;; extract the sub-arguments within the expanded argument to the function; if one
			;; is a scalar value, the function may be merged into the containing closure
			(let ((sub-arg1 (if (and (listp (second arg-expanded))
						 (eql 'lambda (caadr arg-expanded)))
					    (third (third (second arg-expanded)))
					    (third arg-expanded)))
			      (sub-arg2 (if (and (listp (second arg-expanded))
						 (eql 'lambda (caadr arg-expanded)))
					    (fourth (third (second arg-expanded)))
					    (fourth arg-expanded))))
			  ;; one of the sub-arguments must be a number - or if there is no second argument,
			  ;; the inner function is monadic and the decomposition can proceed
			  (or (numberp sub-arg1)
			      (not sub-arg2)
			      (numberp sub-arg2))))
		   (let ((innerfn (second arg-expanded)))
		     (list (if (not (eql 'lambda (first innerfn)))
			       `(lambda (,arg) (funcall ,fn ,@(if (not is-first) (list arg1))
							(funcall ,innerfn ,arg
								 ;; include the inner function's
								 ;; second argument if present
								 ,@(if (fourth arg-expanded)
								       (list (fourth arg-expanded))))
							,@(if is-first (list arg2))))
			       (list (first innerfn) (second innerfn)
				     `(funcall ,fn ,@(if (not is-first) (list arg1))
					       ,(third innerfn) ,@(if is-first (list arg2)))))
			   (third arg-expanded)))))))
      (let* ((scalar-fn (is-scalar function))
	     (fn-body (cond ((and scalar-fn (not (second arguments)))
			     ;; compose monadic functions if the argument is the output of another scalar function
			     (expand-monadic function (first arguments)))
			    ((and scalar-fn (second arguments)
				  (numberp (first arguments)))
			     ;; compose dyadic functions if the first argument is a scalar numeric value
			     ;; and the other argument is the output of a scalar function
			     (let ((expanded (expand-dyadic function nil (first arguments) (second arguments))))
			       (or expanded `((lambda (,arg) (funcall ,function ,(first arguments) ,arg))
					      ,(macroexpand (second arguments))
					      nil))))
			    ((and scalar-fn (second arguments)
				  (numberp (second arguments)))
			     ;; same as above if the numeric argument is reversed
			     (let ((expanded (expand-dyadic function t (first arguments) (second arguments))))
			       (or expanded `((lambda (,arg) (funcall ,function ,arg ,(second arguments)))
					      ,(macroexpand (first arguments))
					      nil))))
			    ;; otherwise, just list the function and its arguments
			    (t (cons function arguments)))))
	(append (list (if scalar-fn 'apply-scalar 'funcall))
		(if (and scalar-fn (= 4 (length fn-body)))
		    ;; if the function is scalar and an axis argument is present,
		    ;; adjust the numerical axis values according to the index origin
		    (append (butlast fn-body 1)
			    `((adjust-axes-for-index-origin index-origin ,(fourth fn-body))))
		    fn-body)
		(if (and scalar-fn (= 2 (length fn-body)))
		    '(nil))
		(if (and scalar-fn (is-boolean function))
		    '(nil t)))))))

#|
This is a minimalistic implementation of (apl-call) that doesn't perform any function composition.
It remains here as a standard against which to compare methods for composing APL functions.

(defmacro apl-call (symbol function &rest arguments)
  (declare (ignore symbol))
  `(,(if (and (listp function)
	      (eql 'scalar-function (first function)))
	 'apply-scalar 'funcall)
     ,function  ,@arguments))
|#

(defmacro apl-compose (symbol &rest body)
  "A wrapper macro for macros that implement April's operators; functionally this macro does nothing but it improves the readability of April's compiled code."
  (declare (ignore symbol))
  (let ((expanded (macroexpand body)))
    (if (or (not (listp (first expanded)))
	    (not (eql 'olambda (caar expanded))))
	expanded (cons 'funcall expanded))))

(defmacro apl-if (&rest each-clause)
  (let ((condition (gensym)))
    (labels ((build-clauses (clauses)
	       `(let ((,condition (disclose-atom ,(first clauses))))
		  (if (not (is-unitary ,condition))
		      (error "Predicates within an [$ if] statement must be unitary or scalar.")
		      (if (/= 0 (disclose-atom ,condition))
			  ,(second clauses)
			  ,(if (third clauses)
			       (if (fourth clauses)
				   (build-clauses (cddr clauses))
				   (third clauses))
			       (make-array nil)))))))
      (build-clauses each-clause))))

(defmacro scalar-function (function)
  "Wrap a scalar function. This is a passthrough macro used by the scalar composition system in (apl-call)."
  (if (symbolp function)
      `(function ,function)
      function))

(defun validate-arg-unitary (value)
  "Verify that a form like (vector 5) represents a unitary value."
  (or (symbolp value)
      (numberp value)
      (and (listp value)
	   (or (not (eql 'vector (first value)))
	       (not (third value))))))

(defmacro or-functional-character (reference symbol)
  "Return a symbol representing a functional character or, if the passed value is not a character, an arbitrary fallback symbol. Used to derive the initial symbol argument for (apl-call)."
  `(if (not (characterp ,reference))
       ,symbol (intern (string-upcase ,reference) ,*package-name-string*)))

(defun enclose-axes (body axis-sets &key (set) (set-by))
  "Apply axes to an array, with the ability to handle multiple sets of axes as in (6 8 5⍴⍳9)[1 4;;2 1][1;2 4 5;]."
  (let ((axes (first axis-sets))
	(assignment-output (gensym)) (assigned-array (gensym)))
    (if (not axis-sets)
	body (enclose-axes
	      (if set `(multiple-value-bind (,assignment-output ,assigned-array)
			   (achoose ,body (mapcar (lambda (array)
						    (if array (apply-scalar #'- array index-origin)))
						  (list ,@axes))
				    :set ,set ,@(if set-by (list :set-by set-by))
				    ;; setting the modify-input parameter so that the original value
				    ;; is modified in place if possible
				    :modify-input t)
			 (if ,assigned-array (setf ,body ,assigned-array))
			 ,assignment-output)
		  `(achoose ,body (mapcar (lambda (array) (if array (apply-scalar #'- array index-origin)))
					  (list ,@axes))))
	      (rest axis-sets)))))

(defun coerce-type (array type-index)
  "Create an array with a numerically designated type holding the contents of the given array."
  (let ((type (case type-index (0 t) (-1 'bit) (1 '(unsigned-byte 2)) (2 '(unsigned-byte 4))
		    (-3 '(unsigned-byte 7)) (3 '(unsigned-byte 8)) (-4 '(unsigned-byte 15))
		    (4 '(unsigned-byte 16)) (-5 '(unsigned-byte 31)) (5 '(unsigned-byte 32))
		    (-6 '(unsigned-byte 63)) (6 '(unsigned-byte 64))
		    (13 '(signed-byte 8)) (14 '(signed-byte 16)) (15 '(signed-byte 32)) 
		    (-16 '(signed-byte 63)) (16 '(signed-byte 64)) (21 'fixnum) 
		    (31 'short-float) (32 'single-float)
		    (34 'double-float) (35 'long-float)
		    (98 'base-char) (99 'character))))
    (if (or (not (arrayp array))
	    (equalp type (element-type array)))
	array (let ((output (make-array (dims array) :element-type type)))
		(dotimes (i (size array)) (setf (row-major-aref output i)
						(row-major-aref array i)))
		output))))

(defun output-value (space form &optional properties)
  "Express an APL value in the form of an explicit array specification or a symbol representing an array, supporting axis arguments."
  (labels ((enclose-symbol (item)
	     ;; enclose the symbol in an (inws) form for interning in the workspace
	     ;; if it isn't one of the designated idiom-native symbols
	     (if (or (not (symbolp item))
		     (member item *idiom-native-symbols*))
		 item `(inws ,item)))
	   (apply-props (item form-props)
	     (let ((form-props (if (not (listp (first form-props)))
				   form-props (first form-props))))
	       (if (getf form-props :axes)
		   (enclose-axes (enclose-symbol item)
				 (getf form-props :axes))
		   (enclose-symbol item)))))
    (let ((properties (reverse properties)))
      (if form (if (listp form)
		   (if (or (eql 'avector (first form))
			   (eql 'inws (first form)))
		       form (if (not (or (numberp (first form))
					 (listp (first form))
					 (stringp (first form))
					 (characterp (first form))
					 (and (arrayp (first form))
					      (= 0 (size (first form))))
					 (member (first form) '(⍺ ⍵ ⍺⍺ ⍵⍵)
						 :test #'eql)
					 (and (not (fboundp (first form)))
					      (and (symbolp (first form))
						   (and (not (fboundp (intern (string (first form))
									      space))))))))
				(if (= 1 (length properties))
				    (apply-props form (first properties))
				    (mapcar #'apply-props form properties))
				(if (getf (first properties) :vector-axes)
				    (enclose-axes `(avector ,@(mapcar #'apply-props form properties))
						  (getf (first properties) :vector-axes))
				    `(avector ,@(mapcar #'apply-props form properties)))))
		   (if (not (numberp form))
		       (apply-props form properties)
		       form))))))

(defun output-function (form &optional arguments assigned-symbols arg-symbols)
  "Express an APL inline function like {⍵+5}."
  (let ((assigned-symbols (loop :for sym :in assigned-symbols
			     :when (not (member (string-upcase sym)
						'("*INDEX-ORIGIN*" "*COMPARISON-TOLERANCE*")
						:test #'string=))
			     :collect `((inws ,sym))))
	(arguments (if arguments (mapcar (lambda (item) `(inws ,item)) arguments))))
    (funcall (if (not (intersection arg-symbols '(⍺⍺ ⍵⍵)))
		 ;; the latter case wraps a user-defined operator
		 #'identity (lambda (form) `(olambda (⍺⍺ &optional ⍵⍵)
					      (declare (ignorable ⍺⍺ ⍵⍵))
					      ,form)))
	     `(alambda ,(if arguments arguments `(⍵ &optional ⍺))
		(declare (ignorable ,@(if arguments arguments '(⍵ ⍺))))
		,@(if (not assigned-symbols)
		      form `((let ,assigned-symbols ,@form)))))))

(defun build-variable-declarations (input-vars space)
  "Create the set of variable declarations that begins April's compiled code."
  (loop :for var-entry :in input-vars :collect (list (intern (lisp->camel-case (first var-entry)) space)
						     (second var-entry))))

(defun build-compiled-code (exps workspace-symbols options system-vars vars-declared stored-refs space)
  "Return a set of compiled April expressions within the proper context."
  (let* ((branch-index (gensym "A")) (branches-sym (intern "*BRANCHES*" space))
	 (tags-found (loop :for exp :in exps :when (symbolp exp) :collect exp))
	 (tags-matching (loop :for tag :in (symbol-value branches-sym)
			   :when (or (and (listp tag) (member (second tag) tags-found))) :collect tag)))
    ;; create an lparallel kernel if none is present; this is done at runtime so April's compilation
    ;; doesn't entail the creation of a kernel
    (flet ((process-tags (form)
	     (loop :for sub-form :in form
		:collect (if (not (and (listp sub-form) (eql 'go (first sub-form))
				       (not (symbolp (second sub-form)))))
			     sub-form (if (integerp (second sub-form))
					  (if (assoc (second sub-form) tags-matching)
					      (list 'go (second (assoc (second sub-form) tags-matching))))
					  (if (third sub-form)
					      `(let ((,branch-index ,(third sub-form)))
						 (cond ,@(loop :for tag :in (second sub-form)
							    :counting tag :into tix
							    :collect `((= ,branch-index ,tix)
								       (go ,tag)))))
					      `(let ((,branch-index ,(second sub-form)))
						 (cond ,@(loop :for tag :in tags-matching
							    :when (and (listp tag)
								       (member (second tag) tags-found))
							    :collect `((= ,branch-index ,(first tag))
								       (go ,(second tag))))))))))))
      (funcall (lambda (code) (if (not (assoc :compile-only options))
				  code `(quote ,code)))
	       (if (or system-vars vars-declared)
		   `(in-april-workspace ,(or (second (assoc :space options)) 'common)
		      (let* (,@(loop :for var :in system-vars
				  :when (not (member (string-upcase (first var)) workspace-symbols
						     :test #'string=))
				  :collect var)
			     ,@vars-declared)
			(declare (ignorable ,@(loop :for var :in system-vars
						 :when (not (member (string-upcase (first var))
								    workspace-symbols :test #'string=))
						 :collect (first var))))
			(symbol-macrolet ,(loop :for var :in system-vars
					     :when (member (string-upcase (first var)) workspace-symbols
							   :test #'string=)
					     :collect var)
			  ,@(loop :for ref :in stored-refs
			       :collect (list (first ref)
					      (list 'inws (second ref)) (third ref)))
			  ,@(if (or (not tags-found) (not (boundp branches-sym)))
				exps `((tagbody ,@(butlast (process-tags exps) 1))
				       ,(first (last exps)))))))
		   (if (< 1 (length exps))
		       (cons 'progn exps) (first exps)))))))

(defun generate-function-retriever (operand axes)
  "This function is used at compile time to generate the functon invoked by the [⍣ power] operator at runtime to fetch a regular or inverse function depending on the right operand passed to it."
  (let ((is-dyadic (gensym)) (is-inverse (gensym)))
    (if (or (symbolp operand) (characterp operand))
	(let* ((dyinv-forms (resolve-function :dyadic-inverse operand axes))
	       (left-fn-monadic
		`(λω (apl-call ,operand ,(resolve-function :monadic operand axes) omega)))
	       (left-fn-dyadic
		`(λωα (apl-call ,operand ,(resolve-function :dyadic operand axes) omega alpha)))
	       (left-fn-monadic-inverse
		`(λω (apl-call ,operand ,(resolve-function :monadic-inverse operand axes) omega)))
	       (left-fn-dyadic-inverse
		`(λωα (apl-call ,operand ,(or (getf dyinv-forms :plain)) omega alpha))))
	  `(lambda (,is-dyadic ,is-inverse)
	     (if (not ,is-inverse) (if ,is-dyadic ,left-fn-dyadic ,left-fn-monadic)
		 (if ,is-dyadic ,left-fn-dyadic-inverse ,left-fn-monadic-inverse))))
	(or (match operand ((list 'function (list 'inws (guard symbol (symbolp symbol))))
			    (let ((inverse-operand
				   `(function (inws ,(intern (concatenate
							      'string "𝕚∇" (string symbol)))))))
			      `(lambda (,is-dyadic ,is-inverse)
				 (declare (ignore ,is-dyadic))
				 (if ,is-inverse ,inverse-operand ,operand)))))
	    (let ((inverted (invert-function operand)))
	      `(lambda (,is-dyadic ,is-inverse)
		 (declare (ignore ,is-dyadic))
		 (if ,is-inverse ,inverted ,operand)))))))
  
(defun assign-self-refs-among-tokens (tokens function)
  "Find a list of symbols within a token list which are assigned with the [← gets] lexical function. Used to find lists of variables to hoist in lambda forms."
  (loop :for token :in tokens :for tx :from 0
     :do (if (and (listp token) (not (member (first token) '(:fn :op))))
	     ;; recursively descend into lists, but not functions contained within a function
	     (assign-self-refs-among-tokens token function)
	     (funcall function token tokens tx))))

(defun get-assigned-symbols (tokens space &optional token-list is-nested is-strand-assignment)
  "Find a list of symbols within a token list which are assigned with the [← gets] lexical function. Used to find lists of variables to hoist in lambda forms."
  (let ((previous-token)
	(token-list (or token-list (list :tokens))))
    (loop :for token :in tokens
       :do (if (and (listp token) (not (member (first token) '(:fn :op))))
	       ;; recursively descend into lists, but not functions contained within a function,
	       ;; otherwise something like {÷{⍺⍺ ⍵}5} will be read as an operator because an inline
	       ;; operator is within it
	       (glean-symbols-from-tokens token space token-list t
					  (and (listp previous-token)
					       (characterp (second previous-token))
					       (char= #\← (second previous-token))
					       (loop :for tk :in token :always (symbolp tk))))
	       (if (and (not (keywordp token))
			(symbolp token))
		   (cond ((and (not (member token *idiom-native-symbols*))
			       (not (member token token-list))
			       (not (boundp (intern (string token) space)))
			       (or is-strand-assignment
				   (and (listp previous-token)
					(eql :fn (first previous-token))
					(characterp (second previous-token))
					(char= #\← (second previous-token)))))
			  (setf (rest token-list)
				(cons token (rest token-list)))))))
	 (setq previous-token token))
    (if is-nested token-list (remove-duplicates (rest token-list)))))

(defun invert-function (form &optional to-wrap)
  "Invert a function expression. For use with the [⍣ power] operator taking a negative right operand."
  (match form
    ((list* 'apl-compose '⍣ 'operate-to-power degree rest)
     ;; invert a [⍣ power] operation - all that needs be done is negate the right operand
     `(apl-compose ⍣ operate-to-power (- ,degree) ,@rest))
    ((list* 'apl-compose '∘ 'operate-composed
	    (list 'apl-compose '\. 'lambda '(o a)
		  (list 'array-outer-product 'o 'a (guard opfn (and (eql 'λωα (first opfn))
								    (eql 'apl-call (caadr opfn))))))
	    _ _ rest)
     ;; invert a right-composition of an [∘. outer product] operation
     `(apl-compose ∘ operate-composed :inverted-op nil
		     (λωα (inverse-outer-product omega (λωα ,(invert-function (second opfn)))
						 nil alpha))
		     ,@rest))
    ((list* 'apl-compose '∘ 'operate-composed op1 nil nil
	    (list 'apl-compose '\. 'lambda '(o a)
		  (list 'array-outer-product 'o 'a (guard opfn (and (eql 'λωα (first opfn))
								    (eql 'apl-call (caadr opfn))))))
	    _ _ rest)
     ;; invert a left-composition of an [∘. outer product] operation
     `(apl-compose ∘ operate-composed ,op1 nil nil :inverted-op nil
		     (λωα (inverse-outer-product alpha (λωα ,(invert-function (second opfn)))
						omega))
		     ,@rest))
    ((list* 'apl-compose '∘ 'operate-composed (guard op1 (not (characterp op1)))
	    nil nil op2-sym _ _ remaining)
     ;; invert a [∘ compose] operation with a value on the left
     (let ((dyinv-forms (resolve-function :dyadic-inverse op2-sym)))
       `(apl-compose ∘ operate-composed ,op1 nil nil ,op2-sym nil
		       (λωα (apl-call ,op2-sym ,(getf dyinv-forms :right-composed) omega alpha))
		       ,@remaining)))
    ((list* 'apl-compose '∘ 'operate-composed op1-sym _ _ (guard op2 (not (characterp op2)))
	    nil nil remaining)
     ;; invert a [∘ compose] operation with a value on the right
     (let ((dyinv-forms (resolve-function :dyadic-inverse op1-sym)))
       `(apl-compose ∘ operate-composed ,op1-sym nil
		       (λωα (apl-call ,op1-sym ,(getf dyinv-forms :plain) omega alpha))
		       ,op2 nil nil ,@remaining)))
    ((list* 'apl-compose '∘ 'operate-composed right-fn-sym right-fn-form-monadic right-fn-form-dyadic
	    left-fn-sym left-fn-form-monadic left-fn-form-dyadic remaining)
     ;; invert a [∘ compose] operation with two function operands
     (let ((left-clause
	    (if (or (eq :fn left-fn-sym)
		    (not (symbolp left-fn-sym)))
		(list left-fn-sym (invert-function left-fn-form-monadic)
		      (invert-function left-fn-form-dyadic))
		(let ((fn-glyph (aref (string left-fn-sym) 0)))
		  (list left-fn-sym
			(if (resolve-function :monadic-inverse fn-glyph)
			    `(λω (apl-call ,left-fn-sym ,(resolve-function :monadic-inverse fn-glyph)
					   omega)))
			(if (resolve-function :dyadic-inverse fn-glyph)
			    `(λωα (apl-call ,left-fn-sym ,(resolve-function :dyadic-inverse fn-glyph)
					    omega alpha)))))))
	   (right-clause
	    (if (or (eq :fn right-fn-sym)
		    (not (symbolp right-fn-sym)))
		(list right-fn-sym (invert-function right-fn-form-monadic)
		      (invert-function right-fn-form-dyadic))
		(let ((fn-glyph (aref (string right-fn-sym) 0)))
		  (list right-fn-sym
			(if (resolve-function :monadic-inverse fn-glyph)
			    `(λω (apl-call ,right-fn-sym ,(resolve-function :monadic-inverse fn-glyph)
					   omega)))
			(if (resolve-function :dyadic-inverse fn-glyph)
			    `(λωα (apl-call ,right-fn-sym ,(resolve-function :dyadic-inverse fn-glyph)
					    omega alpha))))))))
       (if (and (listp (first right-clause))
		(listp (first left-clause)))
	   `(apl-compose ∘ operate-composed ,@left-clause ,@right-clause ,@remaining)
	   `(lambda (omega &optional alpha)
	      (if (not alpha)
		  (funcall (apl-compose ∘ operate-composed ,@left-clause ,@right-clause ,@remaining)
			   omega)
       		  (apl-call ,(if (listp (first right-clause))
				 :fn (first right-clause))
			    ,(if (listp (first right-clause))
			    	 (third (second (second right-clause)))
			    	 (resolve-function :monadic-inverse (first right-clause)))
       			    ,(if (listp (first left-clause))
       				 (third (third (second (third left-clause))))
       				 `(apl-call ,(if (listp (first left-clause))
						 :fn (first left-clause))
       					    ,(getf (resolve-function :dyadic-inverse (first left-clause))
       						   :plain)
       					    omega alpha))))))))
    ((list* 'apl-compose '\\ 'operate-scanning operand remaining)
     ;; invert a [\ scan] operation
     `(apl-compose \\ operate-scanning ,(invert-function operand) ,@remaining t))
    ((list 'apl-compose '\¨ 'operate-each op-monadic op-dyadic)
     ;; invert an [¨ each] operation
     `(apl-compose \¨ operate-each ,(invert-function op-monadic)
		   ,(invert-function op-dyadic)))
    ((list 'apl-compose '⍨ 'lambda args funcall-form)
     ;; invert a [⍨ commute] operation
     (or (match funcall-form ((list* 'funcall (guard sub-lambda (eql 'λωα (first sub-lambda)))
    				     _)
			      (let* ((fn-glyph (second (second sub-lambda)))
				     (dyinv-forms (resolve-function
						   :dyadic-inverse (aref (string fn-glyph) 0))))
				`(apl-compose ⍨ lambda ,args
						(funcall (λω (apl-call ,fn-glyph
									,(getf dyinv-forms :commuted)
									omega alpha))
							 omega)))))
    	 (error "Composition with ⍨ not invertable.")))
    ((list (guard first (member first '(λω λωα))) second)
     ;; invert a λω or λωα macro lambda expression
     (list first (invert-function second)))
    ((list* 'alambda args (guard declare-form (and (listp declare-form) (eql 'declare (first declare-form))))
	    first-form rest-forms)
     ;; invert an arbitrary lambda
     (if rest-forms `(lambda ,args ,declare-form
			     (error "This function has more than one statement and thus cannot be inverted."))
	 `(alambda ,args ,declare-form ,(invert-function first-form))))
    ((list* 'alambda args first-form rest-forms)
     ;; invert an arbitrary lambda
     (if rest-forms `(lambda ,args (declare (ignore ⍵ ⍺))
    			     (error "This function has more than one statement and thus cannot be inverted."))
	 `(alambda ,args ,(invert-function first-form))))
    ((list* 'apl-call function-symbol function-form arg1 arg2-rest)
     (destructuring-bind (&optional arg2 &rest rest) arg2-rest
       ;; invert an apl-call expression - WIP
       (let* ((function-char (aref (string function-symbol) 0))
    	      (dyinv-forms (resolve-function :dyadic-inverse function-char))
    	      (to-invert (or (member arg1 '(⍵ ⍺ omega alpha))
    			     (and (listp arg1) (eql 'apl-call (first arg1)))))
    	      (arg1-var (if to-invert arg1))
    	      (arg2-var (if (or (member arg2 '(⍵ ⍺ omega alpha))
    				(and (listp arg2) (eql 'apl-call (first arg2))))
    			    arg2))
    	      (last-layer (not (or (and (listp arg1) (eql 'apl-call (first arg1)))
    				   (and (listp arg2) (eql 'apl-call (first arg2))))))
    	      (to-wrap (or to-wrap #'identity)))
    	 (flet ((wrapper (item)
    		  `(apl-call ,function-symbol ,(if (eq :fn function-symbol)
    						   (invert-function function-form)
						   (if arg2 (or (if to-invert (getf dyinv-forms :plain)
    								    (or (getf dyinv-forms :right-composed)
    									(getf dyinv-forms :plain)))
    								`(λωα (declare (ignore omega alpha))
    								      (error "No dyadic inverse for ~a."
    									     ,function-char)))
    						       (or (resolve-function :monadic-inverse function-char)
    							   `(λω (declare (ignore omega))
    								(error "No monadic inverse for ~a."
    								       ,function-char)))))
    			     ,@(append (funcall (if (or to-invert (getf dyinv-forms :right-composed))
    						    #'identity #'reverse)
    						(append (list (if (or (listp arg1)
								      (and arg1-var (not arg2-var)))
								  (funcall to-wrap item) arg1))
    							(if (and (not (member arg2 '(⍵ ⍺)))
								 (and arg2-var (not arg1-var)))
							    (list (funcall to-wrap item))
							    (if arg2 (list arg2)))))
    				       rest))))
    	   (if last-layer (wrapper (or arg1-var arg2-var))
    	       (invert-function (or arg1-var arg2-var) #'wrapper))))))))

(defun april-function-glyph-processor (type glyph spec &optional inverse-spec)
  "Convert a Vex function specification for April into a set of lexicon elements, forms and functions that will make up part of the April idiom object used to compile the language."
  (let ((type (intern (string-upcase type) "KEYWORD"))
	(function-type (intern (string-upcase (first spec)) "KEYWORD"))
	(spec-body (rest spec))
	(inverse-spec-body (rest inverse-spec))
	(inverse-function-type (if inverse-spec (intern (string-upcase (first inverse-spec)) "KEYWORD"))))
    (cond ((eq :symbolic function-type)
	   `(,glyph :lexicons (:functions :symbolic-functions)
		    :functions (:symbolic ,(first spec-body))))
	  ((keywordp (first spec-body))
	   ;; if this is a simple scalar declaration passing through another function
	   `(,glyph :lexicons (:functions :scalar-functions :monadic-functions :scalar-monadic-functions
					  ,@(if (not (eq :monadic function-type))
						'(:dyadic-functions :scalar-dyadic-functions))
					  ,@(if (and inverse-function-type
						     (not (eq :dyadic inverse-function-type)))
						'(:inverse-monadic-functions
						  :scalar-inverse-monadic-functions))
					  ,@(if (or (eq :dyadic inverse-function-type)
						    (eq :ambivalent inverse-function-type))
						'(:inverse-dyadic-functions
						  :scalar-inverse-dyadic-functions)))
		    :functions ,(append (if (or (eq :ambivalent function-type)
						(eq :monadic function-type))
					    (list :monadic `(scalar-function ,(second spec-body))))
					(if (or (eq :ambivalent function-type)
						(eq :dyadic function-type))
					    (list :dyadic `(scalar-function ,(first (last spec-body)))))
					(if (or (eq :ambivalent inverse-function-type)
						(eq :monadic inverse-function-type))
					    (list :monadic-inverse
						  `(scalar-function ,(first inverse-spec-body))))
					(if (or (eq :ambivalent inverse-function-type)
						(eq :dyadic inverse-function-type))
					    (list :dyadic-inverse
						  (funcall (lambda (spec)
							     (loop :for (key val) :on spec :by #'cddr
								:append (list key `(scalar-function ,val))))
							   (if (eq :dyadic inverse-function-type)
							       inverse-spec-body (rest inverse-spec-body)))))
					)))
	  (t `(,glyph :lexicons ,(cond ((eq :functions type)
					`(:functions ,@(if (eq :ambivalent function-type)
							   '(:monadic-functions :dyadic-functions)
							   (list (intern (string-upcase
									  (concatenate 'string
										       (string function-type)
										       "-" (string type)))
									 "KEYWORD")))
						     ,@(if (eq :ambivalent inverse-function-type)
							   '(:inverse-monadic-functions
							     :inverse-dyadic-functions)
							   (list (intern (string-upcase
									  (concatenate
									   'string
									   (string inverse-function-type)
									   "-" (string type)))
									 "KEYWORD")))
						     ,@(if (and (or (eq :ambivalent function-type)
								    (eq :monadic function-type))
								(eql 'scalar-function (caar spec-body)))
							   '(:scalar-functions :scalar-monadic-functions))
						     ,@(if (or (and (eq :dyadic function-type)
								    (eql 'scalar-function (caar spec-body)))
							       (and (eq :ambivalent function-type)
								    (eql 'scalar-function (caadr spec-body))))
							   '(:scalar-functions :scalar-dyadic-functions))
						     ,@(if (and (or (eq :ambivalent inverse-function-type)
								    (eq :monadic inverse-function-type))
								(eql 'scalar-function
								     (caar inverse-spec-body)))
							   '(:inverse-scalar-functions
							     :inverse-scalar-monadic-functions))
						     ,@(if (or (and (eq :dyadic inverse-function-type)
								    (listp (car inverse-spec-body))
								    (eql 'scalar-function
									 (caar inverse-spec-body)))
							       (and (eq :ambivalent function-type)
								    (listp (cadr inverse-spec-body))
								    (eql 'scalar-function
									 (caadr inverse-spec-body))))
							   '(:inverse-scalar-functions
							     :inverse-scalar-dyadic-functions))))
				       ((eq :operators type)
					`(:operators ,(if (eq :lateral function-type)
							  :lateral-operators
							  (if (eq :pivotal function-type)
							      :pivotal-operators :unitary-operators)))))
		      ,@(cond ((eq :functions type)
			       `(:functions ,(append (if (or (eq :ambivalent function-type)
							     (eq :monadic function-type))
							 (list :monadic (first spec-body)))
						     (if (eq :ambivalent function-type)
							 (list :dyadic (second spec-body))
							 (if (eq :dyadic function-type)
							     (list :dyadic (first spec-body))))
						     (if (or (eq :ambivalent inverse-function-type)
							     (eq :monadic inverse-function-type))
							 (list :monadic-inverse
							       (first inverse-spec-body)))
						     (if (eq :ambivalent inverse-function-type)
							 (list :dyadic-inverse (rest inverse-spec-body))
							 (if (eq :dyadic inverse-function-type)
							     (list :dyadic-inverse inverse-spec-body))))))
			      ((eq :operators type)
			       `(:operators ,(first spec-body)))))))))

;; a secondary package containing a set of tools for the extension of April idioms
(defpackage #:april.idiom-extension-tools
  (:import-from :april #:extend-vex-idiom #:april-function-glyph-processor #:scalar-function)
  (:export #:extend-vex-idiom #:april-function-glyph-processor #:scalar-function
	   #:λω #:λωα #:λωχ #:λωαχ))


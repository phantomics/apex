;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; utilities.lisp

(in-package #:april)

(define-symbol-macro this-idiom (local-idiom april))
(define-symbol-macro atomic-vector (of-system this-idiom :atomic-vector))
(define-symbol-macro *apl-timestamp* (apl-timestamp))
(define-symbol-macro *first-axis* (if axes (- (aref (first axes) 0)
					      index-origin)
				      0))
(define-symbol-macro *last-axis* (- (if axes (aref (first axes) 0)
					(rank omega))
				    index-origin))
(define-symbol-macro *first-axis-or-nil* (if axes (- (aref (first axes) 0)
						     index-origin)))

(defmacro ∘Ω (&rest body)
  `(lambda (omega) ,@body))

(defmacro ∘ΩΑ (&rest body)
  `(lambda (omega alpha) ,@body))

(defmacro ∘ΩΑΧ (&rest body)
  `(lambda (omega alpha &optional axes) ,@body))

(defmacro ∘ΩΧ (&rest body)
  `(lambda (omega &optional axes) ,@body))

(defmacro print-and-run (form)
  `(progn (princ (indent-code (string-downcase (write-to-string (quote ,form)))))
	  ,form))

(defun indent-code (string)
  (concatenate 'string "  * " (regex-replace-all "[\\n]" string (concatenate 'string '(#\Newline)  "    "))))

(defun disclose-atom (item)
  "If the argument is a non-nested array with only one member, disclose it, otherwise do nothing."
  (if (and (not (stringp item))
	   (arrayp item)
	   (is-unitary item)
	   (not (arrayp (row-major-aref item 0))))
      (row-major-aref item 0)
      item))

(defmacro in-apl-workspace (workspace-symbol body)
  "This macro encloses a body of compiled April code specifying a workspace in use for the code, and extends any assignment so as to update the workspace's stored values."
  (labels ((process (form)
	     (loop :for item :in form
		:collect (if (and (listp item) (eql 'apl-assign (first item)))
			     (cond ((or (eql 'index-origin (second item))
					(eql 'print-precision (second item)))
				    ;; if it's a system variable, assign the corresponding value
				    ;; in the workspace as well as in the lexical environment
				    `(progn ,(macroexpand item)
					    (setf (getf (getf (gethash :system ,workspace-symbol) :state)
							,(intern (string-upcase (second item)) "KEYWORD"))
						  ,(second (third item)))))
				   (t (list 'setq (second item)
					    `(setf (gethash ',(second item)
							    (gethash :values ,workspace-symbol))
						   ,(if (listp (third item))
							(process (third item))
							(third item))))))
			     (if (listp item)
				 (process item)
				 item)))))
    (process body)))

(defmacro apl-assign (symbol value)
  "This is a simple passthrough macro that is used by (in-apl-workspace)."
  (cond ((or (eql 'index-origin symbol)
	     (eql 'print-precision symbol))
	 ;; TODO: add better logic for invalid assignments to ⎕IO or ⎕PP
	 (if (and (eql 'avector (first value))
		  (integerp (second value)))
	     `(setq ,symbol ,(second value))
	     (error "Attempted to assign invalid value to [⎕IO Index Origin].")))
	(t `(setq ,symbol ,value))))

(defmacro apl-output (form &rest options)
  "Generate code to output the result of APL evaluation, with options to print an APL-formatted text string expressing said result and/or return the text string as a result."
  (let ((result (gensym)) (printout (gensym)))
    `(let* ((,result ,form)
	    (,printout ,(if (or (getf options :print-to)
				(getf options :output-printed))
			    `(matrix-print ,result :append #\Newline
					   :format (lambda (n) (print-apl-number-string
								n t ,(getf options :print-precision)))))))
       (declare (ignorable ,printout))
       ;; TODO: add printing rules for functions like {⍵+1}
       ;(if (arrayp ,result)
       ,(if (getf options :print-to)
	    (let ((string-output `(write-string ,printout ,(getf options :print-to))))
	      `(if (arrayp ,result)
		   ,string-output (concatenate 'string ,string-output (list #\Newline)))))
       ,(if (getf options :output-printed)
	    (if (eq :only (getf options :output-printed))
		printout `(values ,result ,printout))
	    result))))

(defun array-to-nested-vector (array)
  "Convert an array to a nested vector. Useful for applications such as JSON conversion where multidimensional arrays must be converted to nested vectors."
  (aops:each (lambda (member) (if (not (and (arrayp member) (< 1 (rank member))))
				  member (array-to-nested-vector member)))
	     (aops:split array 1)))

(defmacro avector (&rest items)
  "This macro returns an APL vector, disclosing data within that are meant to be individual atoms."
  (let* ((type))
    (loop :for item :in items :while (not (eq t type))
       :do (let ((item (if (not (listp item))
			   item (if (not (eql 'avatom (first item)))
				    item (second item)))))
	     (setq type (type-in-common type (assign-element-type item)))))
    `(make-array (list ,(length items))
		 :element-type (quote ,type)
		 :initial-contents (list ,@(loop :for item :in items
					      :collect (if (and (listp item)
								(eql 'avatom (first item)))
							   `(disclose ,item)
							   item))))))

(defmacro avatom (item)
  "An APL vector atom. This passthrough macro provides information to the (avector) macro."
  item)

(defun apply-scalar (function alpha &optional omega is-boolean)
  "Apply a scalar function across objects as appropriate for APL. Handles scalars as well as nested and multidimensional arrays."
  (macrolet ((for-each (function &rest body)
	       `(funcall (if (and is-boolean (not (eq t (element-type alpha)))
				  (or (not omega)
				      (not (eq t (element-type omega)))))
			     #'each-boolean #'each-scalar)
			 (lambda (elem coords)
			   (declare (ignorable coords))
			   ,(if (second body)
				`(funcall ,function elem (apply #'aref (cons ,(second body) coords)))
				`(funcall ,function elem)))
			 ,(first body))))
    (if (not omega)
	(let ((omega alpha))
	  (if (arrayp omega)
	      (labels ((apply-fn (arg)
			 (if (arrayp arg) (apply-scalar #'apply-fn arg nil is-boolean)
			     (funcall function arg))))
		(for-each #'apply-fn omega))
	      (funcall function omega)))
	(let* ((alpha-scalar? (not (arrayp alpha)))
	       (omega-scalar? (not (arrayp omega)))
	       (alpha-unitary? (or alpha-scalar? (is-unitary alpha)))
	       (omega-unitary? (or omega-scalar? (is-unitary omega))))
	  (cond ((and alpha-scalar? omega-scalar?)
		 (funcall function alpha omega))
		((and alpha-scalar? omega-unitary?)
		 (disclose-atom (for-each (lambda (a o) (apply-scalar function a o is-boolean))
					  (vector alpha) omega)))
		((and alpha-unitary? omega-scalar?)
		 (disclose-atom (for-each (lambda (a o) (apply-scalar function a o is-boolean))
					  alpha (vector omega))))
		((and alpha-unitary? omega-unitary?)
		 (for-each (lambda (a o) (apply-scalar function a o is-boolean))
			   alpha omega))
		((not (or alpha-unitary? omega-unitary? alpha-scalar? omega-scalar?))
		 (if (loop for a in (dims alpha) and o in (dims omega)
			:always (= a o))
		     (for-each (lambda (alpha omega) (apply-scalar function alpha omega is-boolean))
			       alpha omega)
		     (error "Array size mismatch.")))
		(t (labels ((scan-over (element)
			      (if (arrayp element)
				  (for-each #'scan-over element)
				  (multiple-value-bind (left right)
				      (cond (alpha-scalar? (values alpha element))
					    (alpha-unitary? (values (disclose alpha) element))
					    (omega-scalar? (values element omega))
					    (omega-unitary? (values element (disclose omega))))
				    (apply-scalar function left right is-boolean)))))
		     (for-each #'scan-over (if (or alpha-scalar? alpha-unitary?)
					       omega alpha)))))))))

(defun numeric-string-p (string)
  "Checks whether the argument is a numeric string."
  (ignore-errors (parse-apl-number-string string)))

(defun parse-apl-number-string (number-string &optional imaginary-component)
  "Parse an APL numeric string into a Lisp value, handling high minus signs and the J-notation for complex numbers."
  (let ((nstring (string-upcase number-string)))
    (if (and (not imaginary-component)
	     (find #\J nstring))
	(let ((halves (cl-ppcre:split "J" nstring)))
	  (if (and (= 2 (length halves))
		   (< 0 (length (first halves)))
		   (< 0 (length (second halves))))
	      (complex (parse-apl-number-string (first halves) t)
		       (parse-apl-number-string (second halves) t))))
	;; the macron character is converted to the minus sign
	(parse-number:parse-number (regex-replace-all "[¯]" nstring "-")))))

(defun print-apl-number-string (number &optional coerce-rational precision decimals)
  "Format a number as appropriate for APL, using high minus signs and J-notation for complex numbers, optionally at a given precision for floats."
  (cond ((complexp number)
	 (format nil "~aJ~a" (print-apl-number-string (realpart number) coerce-rational precision)
		 (print-apl-number-string (imagpart number) coerce-rational precision)))
	((> 0 number)
	 (format nil "¯~a" (print-apl-number-string (abs number) coerce-rational precision)))
	((integerp number)
	 (format nil "~D" number))
	((and coerce-rational (rationalp number))
	 (let ((before-decimal (max 1 (1+ (floor (log number 10))))))
	   (format-decimal-number number :round-magnitude (min 0 (- (- precision before-decimal))))))
	((rationalp number)
	 (write-to-string number))
	(t (if (not precision)
	       (format nil "~F" number)
	       (let ((printed (if (not decimals)
				  (loop :for digit :across (write-to-string number) :when (digit-char-p digit)
				     :counting digit :into digits :finally (return digits))))
		     (before-decimal (max 1 (1+ (floor (log number 10))))))
		 (format nil (format nil "~~~D,~D,F" (if decimals (+ 1 before-decimal decimals)
							 (min (1+ printed) (1+ precision)))
				     (if decimals decimals (- (min printed precision) before-decimal)))
			 number))))))

(defun format-value (idiom-name meta symbols element)
  "Convert a token string into an APL value, paying heed to APL's native ⍺, ⍵ and ⍬ variables."
  (cond ((string= element "⍬")
	 ;; APL's "zilde" character yields a keyword the compiler translates to an empty vector
	 :empty-array)
	((or (and (char= #\" (aref element 0))
		  (char= #\" (aref element (1- (length element)))))
	     (and (char= #\' (aref element 0))
		  (char= #\' (aref element (1- (length element))))))
	 ;; strings are converted to Lisp strings and passed through
	 (subseq element 1 (1- (length element))))	       
	((or (string= element "⍺")
	     (string= element "⍵"))
	 ;; alpha and omega characters are directly changed to symbols
	 (intern element idiom-name))
	((numeric-string-p element)
	 (parse-apl-number-string element))
	(t (let ((vars-table (gethash :variables meta))
		 (elem-keyword (intern element "KEYWORD")))
	     (or (and (char= #\⎕ (aref element 0))
		      (or (getf (rest (assoc :variable symbols))
				(intern (string-upcase element) "APRIL"))
			  (getf (rest (assoc :constant symbols))
				(intern (string-upcase element) "APRIL"))))
		 (if (not vars-table)
		     (setf vars-table (make-hash-table :test #'eq)))
		 (let ((variable-found (gethash elem-keyword vars-table)))
		   (if variable-found variable-found
		       ;; create a new variable if no variable is found matching the string
		       (setf (gethash elem-keyword vars-table)
			     (gensym "A")))))))))

(defun apl-timestamp ()
  "Generate an APL timestamp, a vector of the current year, month, day, hour, minute, second and millisecond."
  (let ((now (now)))
    (make-array '(7) :element-type '(integer 0 16384)
		:initial-contents (list (year-of now) (month-of now) (day-of now) (hour-of now)
					(minute-of now) (second-of now) (millisecond-of now)))))

(defun process-output-vector (items)
  "Process items in a vector to be generated by the compiler, wrapping any array references in aplSymbol so that they are disclosed. This does not apply if the output vector is unitary (length 1)."
  (loop :for item :in items :collect (if (and (< 1 (length items))
					      (listp item) (eql 'choose (first item)))
					 (list 'disclose item)
					 item)))

(defmacro verify-function (reference)
  "Verify that a function exists, either in the form of a character-referenced function, an explicit inline function or a user-created symbol referencing a function."
  `(if (characterp ,reference)
       (or (of-functions this-idiom ,reference :monadic)
	   (of-functions this-idiom ,reference :dyadic)
	   (of-functions this-idiom ,reference :symbolic))
       (if (symbolp ,reference)
	   (if (gethash ,reference (gethash :functions workspace))
	       ,reference)
	   (if (and (listp ,reference)
		    (eql 'lambda (first ,reference)))
	       ,reference))))

(defmacro resolve-function (mode reference)
  "Retrieve function content for a functional character, pass through an explicit or symbol-referenced function, or return nil if the function doesn't exist."
  `(if (characterp ,reference)
       (of-functions this-idiom ,reference ,mode)
       (if (symbolp ,reference)
	   (if (gethash ,reference (gethash :functions workspace))
	       ,reference)
	   (if (and (listp ,reference)
		    (eql 'lambda (first ,reference)))
	       ,reference))))

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
			  (or (or (numberp sub-arg1)
				  (and (listp sub-arg1)
				       (eql 'avatom (first sub-arg1))
				       (numberp (second sub-arg1))))
			      (not sub-arg2)
			      (or (numberp sub-arg2)
				  (and (listp sub-arg2)
				       (eql 'avatom (first sub-arg2))
				       (numberp (second sub-arg2)))))))
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
	     (arguments (loop :for arg :in arguments
	     		   :collect (cond ((not (listp arg))
	     				   `(enclose-atom ,arg))
	     				  ((eql 'avatom (first arg))
	     				   `(enclose-atom ,(second arg)))
	     				  (t arg))))
	     (fn-body (cond ((and scalar-fn (not (second arguments)))
			     ;; compose monadic functions if the argument is the output of another scalar function
			     (expand-monadic function (first arguments)))
			    ((and scalar-fn (second arguments)
				  (listp (first arguments))
				  (eql 'avector (caar arguments))
				  (not (third (first arguments)))
				  (numberp (cadar arguments)))
			     ;; compose dyadic functions if the first argument is a scalar numeric value
			     ;; and the other argument is the output of a scalar function
			     (let ((expanded (expand-dyadic function nil (cadar arguments) (second arguments))))
			       (or expanded `((lambda (,arg) (funcall ,function ,(cadar arguments) ,arg))
					      ,(macroexpand (second arguments))
					      nil))))
			    ((and scalar-fn (second arguments)
				  (listp (second arguments))
				  (eql 'avector (caadr arguments))
				  (not (third (second arguments)))
				  (numberp (cadadr arguments)))
			     ;; same as above if the numeric argument is reversed
			     (let ((expanded (expand-dyadic function t (first arguments) (cadadr arguments))))
			       (or expanded `((lambda (,arg) (funcall ,function ,arg ,(cadadr arguments)))
					      ,(macroexpand (first arguments))
					      nil))))
			    ;; otherwise, just list the function and its arguments
			    (t (cons function arguments)))))
	(append (list (if scalar-fn 'apply-scalar 'funcall))
		fn-body (if (and scalar-fn (= 2 (length fn-body)))
			    '(nil))
		(if (and scalar-fn (is-boolean function))
		    '(t)))))))

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
       ,symbol (intern (string-upcase ,reference))))

(defun enclose-axes (body axis-sets &key (set nil))
  "Apply axes to an array, with the ability to handle multiple sets of axes as in (6 8 5⍴⍳9)[1 4;;2 1][1;2 4 5;]."
  (let ((axes (first axis-sets)))
    (if (not axis-sets)
	body (enclose-axes
	      (if set `(multiple-value-bind (assignment-output assigned-array)
			   (choose ,body (mapcar (lambda (array) (if array (apply-scalar #'- array index-origin)))
						 (list ,@axes))
				   :set ,set)
			 (if assigned-array (setf ,body assigned-array)
			     assignment-output))
		  `(choose ,body (mapcar (lambda (array) (if array (apply-scalar #'- array index-origin)))
					 (list ,@axes))))
	      (rest axis-sets)))))

(defun output-value (space form &optional properties)
  "Express an APL value in the form of an explicit array specification or a symbol representing an array, supporting axis arguments."
  (flet ((apply-props (item form-props)
	   (let ((form-props (if (listp (first form-props))
				 (first form-props)
				 form-props)))
	     ;; wrap output symbols in the (avatom) form so that they are disclosed
	     ;; if part of an APL vector (avector)
	     (funcall (if (not (symbolp item))
			  #'identity (lambda (item) `(avatom ,item)))
		      (if (getf form-props :axes)
			  (enclose-axes item (getf form-props :axes))
			  item)))))
    (let ((properties (reverse properties)))
      (if form (if (listp form)
		   (if (not (or (numberp (first form))
				(listp (first form))
				(stringp (first form))
				(eql '⍺ (first form))
				(eql '⍵ (first form))
				(and (symbolp (first form))
				     (or (gethash (string (first form))
						  (gethash :values space))
					 (not (loop :for key :being :the :hash-keys :of (gethash :variables space)
						 :never (eql (first form)
							     (gethash key (gethash :variables space)))))))))
		       (if (= 1 (length properties))
			   (apply-props form (first properties))
			   (mapcar #'apply-props form properties))
		       `(avector ,@(mapcar #'apply-props form properties)))
		   (if (not (numberp form))
		       (apply-props form properties)
		       `(avector ,form)))))))

(defun output-function (form &optional arguments)
  "Express an APL inline function like {⍵+5}."
  `(lambda ,(if arguments arguments `(⍵ &optional ⍺))
     (let ,(if arguments (loop :for arg :in arguments :collect `(,arg (disclose ,arg)))
	       `((⍵ (disclose ⍵))
		 (⍺ (if ⍺ (disclose ⍺)))))
       (declare (ignorable ,@(if arguments arguments `(⍵ ⍺))))
       ,@form)))

(defun without (omega alpha)
  (flet ((compare (o a)
	   (funcall (if (and (characterp a) (characterp o))
			#'char= (if (and (numberp a) (numberp o))
				    #'= (error "Compared incompatible types.")))
		    o a)))
    (let ((included)
	  (omega-vector (if (or (vectorp omega)
				(not (arrayp omega)))
			    (disclose omega)
			    (make-array (list (array-total-size omega))
					:element-type (element-type omega)
					:displaced-to omega))))
      (loop :for element :across alpha
	 :do (let ((include t))
	       (if (vectorp omega-vector)
		   (loop :for ex :across omega-vector
		      :do (if (compare ex element) (setq include nil)))
		   (if (compare omega-vector element) (setq include nil)))
	       (if include (setq included (cons element included)))))
      (make-array (list (length included))
		  :element-type (element-type alpha)
		  :initial-contents (reverse included)))))

(defun count-to (index index-origin)
  "Implementation of APL's ⍳ function."
  (let ((index (disclose index)))
    (if (not (integerp index))
	(error "The argument to ⍳ must be a single integer, i.e. ⍳9.")
	(let ((output (make-array (list index) :element-type (list 'integer 0 index))))
	  (loop :for ix :below index :do (setf (aref output ix) (+ ix index-origin)))
	  output))))

(defun membership (omega alpha)
  (let ((output (make-array (dims alpha) :element-type 'bit :initial-element 0))
	(to-search (make-array (list (array-total-size omega))
			       :displaced-to omega :element-type (element-type omega))))
    ;; TODO: this could be faster with use of a hash table and other additions
    (dotimes (index (array-total-size output))
      (let ((found))
	(loop :for item :across to-search :while (not found)
	   :do (setq found (or (and (numberp item)
				    (numberp (row-major-aref alpha index))
				    (= item (row-major-aref alpha index)))
			       (and (characterp item)
				    (characterp (row-major-aref alpha index))
				    (char= item (row-major-aref alpha index)))
			       (and (arrayp item)
				    (arrayp (row-major-aref alpha index))
				    (array-compare item (row-major-aref alpha index))))))
	(if found (setf (row-major-aref output index) 1))))
    output))

(defun where-equal-to-one (omega index-origin)
  (let* ((indices) (match-count 0)
	 (omega (enclose-atom omega))
	 (orank (rank omega)))
    (across omega (lambda (index coords)
		    (if (= 1 index)
			(let* ((max-coord 0)
			       (coords (mapcar (lambda (i)
						 (setq max-coord
						       (max max-coord (+ i index-origin)))
						 (+ i index-origin))
					       coords)))
			  (incf match-count)
			  (setq indices (cons (if (< 1 orank)
						  (make-array (list orank)
							      :element-type
							      (list 'integer 0 max-coord)
							      :initial-contents coords)
						  (first coords))
					      indices))))))
    (if (not indices)
	(make-array '(0))
	(make-array (list match-count)
		    :element-type (if (< 1 orank)
				      t (list 'integer 0 (reduce #'max indices)))
		    :initial-contents (reverse indices)))))

(defun tabulate (omega)
  (if (not (arrayp omega))
      (enclose omega)
      (if (vectorp omega)
	  (make-array (list (length omega) 1)
		      :element-type (element-type omega)
		      :initial-contents
		      (loop :for i :below (length omega)
			 :collect (list (aref omega i))))
	  (let ((o-dims (dims omega)))
	    (make-array (list (first o-dims) (reduce #'* (rest o-dims)))
			:element-type (element-type omega)
			:displaced-to (copy-array omega))))))

(defun array-intersection (omega alpha)
  (if (or (not (vectorp alpha))
	  (not (vectorp omega)))
      (error "Arguments must be vectors.")
      (let* ((match-count 0)
	     (matches (loop :for item :across alpha :when (find item omega :test #'array-compare)
			 :collect item :and :do (incf match-count))))
	(make-array (list match-count)
		    :element-type (type-in-common (element-type alpha)
						  (element-type omega))
		    :initial-contents matches))))

(defun unique (omega)
  (let ((vector (if (vectorp omega)
		    omega (re-enclose omega (make-array (list (1- (rank omega)))
							:element-type 'fixnum
							:initial-contents
							(loop :for i :from 1 :to (1- (rank omega))
							   :collect i))))))
    (let ((uniques) (unique-count 0))
      (loop :for item :across vector :when (not (find item uniques :test #'array-compare))
	 :do (setq uniques (cons item uniques))
	 (incf unique-count))
      (funcall (if (vectorp omega)
		   #'identity (lambda (output) (mix-arrays (vector 1) output)))
	       (make-array (list unique-count) :element-type (element-type vector)
			   :initial-contents (reverse uniques))))))

(defun array-union (omega alpha)
  (if (or (not (vectorp alpha))
	  (not (vectorp omega)))
      (error "Arguments must be vectors.")
      (let* ((unique-count 0)
	     (uniques (loop :for item :across omega :when (not (find item alpha
								     :test #'array-compare))
			 :collect item :and :do (incf unique-count))))
	(catenate alpha (make-array (list unique-count)
				    :element-type (type-in-common (element-type alpha)
								  (element-type omega))
				    :initial-contents uniques)
		  0))))

(defun encode (omega alpha)
  (flet ((rebase (bases number &optional out-vector)
	   (let ((operand number)
		 (last-base 1)
		 (base 1)
		 (component 1)
		 (element))
	     (loop :for index :from (1- (length bases)) :downto 0
		:do (setq last-base base
			  base (* base (aref bases index))
			  component (if (= 0 base)
					operand (* base (nth-value 1 (floor (/ operand base)))))
			  operand (- operand component)
			  element (/ component last-base))
		(setf (aref out-vector index) element)))))
    (if (is-unitary omega)
	(let ((out-vector (make-array (list (first (dims alpha)))
				      :element-type (element-type alpha))))
	  (rebase alpha (disclose omega) out-vector)
	  out-vector)
	(let* ((d-o (dims omega))
	       (d-a (dims alpha))
	       (adim1 (first d-a))
	       (bases (make-array (list adim1) :element-type (element-type alpha)))
	       (out-vector (make-array (list adim1) :element-type (element-type alpha)))
	       (output (make-array (append (remove 1 d-a)
					   (remove 1 d-o)))))
	  (across omega (lambda (elem-o ocs)
			  (across alpha
				  (lambda (elem-a acs)
				    (declare (ignore elem-a))
				    (loop :for ix :below adim1
				       :do (setf (aref bases ix)
						 (apply #'aref (cons alpha (cons ix (rest acs)))))))
				  :dimensions (cons 1 (rest d-a)))
			  (rebase bases elem-o out-vector)
			  (loop :for ix :below adim1
			     :do (setf (apply #'aref (cons output (cons ix ocs)))
				       (aref out-vector ix)))))
	  (each-scalar t output)))))

(defun decode (omega alpha)
  (flet ((rebase (bases numerators)
	   (let ((result 0) (base 1))
	     (loop :for index :from (1- (length numerators)) :downto 0
		:do (incf result (* base (aref numerators index)))
		(setf base (* base (aref bases index))))
	     result)))
    (let* ((d-o (dims omega))
	   (d-a (dims alpha)))
      (if (and (not (is-unitary alpha))
	       (not (is-unitary omega))
	       (not (= (first d-a) (first (last d-o)))))
	  (error (concatenate 'string "If neither argument to ⊥ is scalar, the first dimension"
			      " of the left argument must equal the last dimension of"
			      "the right argument."))
	  (let* ((numerators (if (not (vectorp omega))
				 (make-array (list (first d-o)) :element-type (element-type alpha))))
		 (omega (if (and (is-unitary omega)
				 (not (is-unitary alpha)))
			    (make-array (list (first d-a)) :element-type (element-type omega)
					:initial-element (disclose omega))
			    omega))
		 (output (if (and (< 1 (rank omega))
				  (< 1 (rank alpha)))
			     (make-array (append (butlast d-a 1)
						 (rest d-o)))))
		 (bases (if output (make-array (list (first (last d-a))) :element-type (element-type alpha))
			    (if (is-unitary alpha)
				(make-array (list (first d-o)) :element-type (element-type alpha)
					    :initial-element (disclose alpha))
				alpha))))
	    (if output (across output (lambda (elem coords)
					(declare (ignore elem))
					(loop :for dm :below (first (last d-a))
					   :do (setf (aref bases dm)
						     (apply #'aref alpha
							    (append (butlast coords (1- (rank output)))
								    (list dm)))))
					(if numerators
					    (loop :for dm :below (first d-o)
					       :do (setf (aref numerators dm)
							 (apply #'aref (cons omega
									     (append (list dm)
										     (nthcdr (1- (rank output))
											     coords)))))))
					(setf (apply #'aref (cons output coords))
					      (rebase bases (if numerators numerators omega))))))
	    (if output (each-scalar t output)
		(if (vectorp omega)
		    (enclose (rebase bases omega))
		    (each-scalar t (aops:margin (lambda (sub-array) (rebase bases sub-array))
						omega 0)))))))))

(defun left-invert-matrix (in-matrix)
  "Perform left inversion of matrix, used in the ⌹ function."
  (let* ((input (if (= 2 (rank in-matrix))
		    in-matrix (make-array (list (length in-matrix) 1)
					  :element-type (element-type in-matrix)
					  :initial-contents (loop :for i :across in-matrix :collect (list i)))))
	 (result (array-inner-product
		  (invert-matrix (array-inner-product (aops:permute (reverse (iota (rank input)))
								    input)
						      input (lambda (arg1 arg2) (apply-scalar #'* arg1 arg2))
						      #'+))
		  (aops:permute (reverse (iota (rank input)))
				input)
		  (lambda (arg1 arg2) (apply-scalar #'* arg1 arg2))
		  #'+)))
    (if (= 1 (rank in-matrix))
	(aref (aops:split result 1) 0)
	result)))

(defun do-over (input function axis &key reduce in-reverse)
  (let ((output (make-array (if reduce (or (loop :for dim :in (dims input) :counting dim :into dx
					      :when (/= dx (1+ axis)) :collect dim)
					   (list 1))
				(dims input)))))
    (across input (lambda (elem coords)
		    (if (= (if (not in-reverse)
			       0 (1- (nth axis (dims input))))
			   (nth axis coords))
			(setf (apply #'aref output (if (not reduce)
						       coords (or (loop :for c :in coords :counting c :into cx
								     :when (/= axis (1- cx))
								     :collect c)
								  (list 0))))
			      (apply #'aref input coords))
			(setf (apply #'aref output (if (not reduce)
						       coords (or (loop :for c :in coords :counting c :into cx
								     :when (/= axis (1- cx))
								     :collect c)
								  (list 0))))
			      (disclose (funcall function
						 (apply #'aref input coords)
						 (apply #'aref output (or (loop :for c :in coords :counting c :into cx
									     :append (if (/= axis (1- cx))
											 (list c)
											 (if (not reduce)
											     (list (1- c)))))
									  (list 0))))))))
	    :reverse-axes (if in-reverse (list axis)))
    (each-scalar t output)))

(defun build-variable-declarations (input-vars preexisting-vars var-symbols meta)
  (let ((declarations (loop :for key-symbol :in var-symbols
			 :when (not (member (string (gethash (first key-symbol) (gethash :variables meta)))
					    (mapcar #'first input-vars)))
			 :collect (let* ((sym (second key-symbol))
					 (fun-ref (gethash sym (gethash :functions meta)))
					 (val-ref (gethash sym (gethash :values meta))))
				    (list sym (if (member sym preexisting-vars)
						  (if val-ref val-ref (if fun-ref fun-ref))
						  :undefined))))))
    ;; update the variable records in the meta object if input variables are present
    (if input-vars (loop :for var-entry :in input-vars
		      :do (symbol-macrolet ((vdata (gethash (intern (lisp->camel-case (first var-entry)) "KEYWORD")
							    (gethash :variables meta))))
			    (if vdata (rplacd (assoc vdata declarations)
					      (list (second var-entry)))
				(setq declarations (append declarations (list (list (setf vdata (gensym))
										    (second var-entry)))))))))
    declarations))

(defun build-compiled-code (exps options system-vars vars-declared var-symbols meta)
  (let ((tb-output (gensym "A"))
	(branch-index (gensym "A")))
    (flet ((process-tags (form tags)
	     (loop :for sub-form :in form
		:collect (if (not (and (listp sub-form) (eql 'go (first sub-form))
				       (not (symbolp (second sub-form)))))
			     sub-form (if (integerp (second sub-form))
					  (if (assoc (second sub-form) tags)
					      (list 'go (second (assoc (second sub-form) tags))))
					  (if (third sub-form)
					      `(let ((,branch-index
						      (row-major-aref ,(third sub-form) 0)))
						 (cond ,@(loop :for tag :in (second sub-form)
							    :counting tag :into tix
							    :collect `((= ,branch-index ,tix)
								       (go ,tag)))))
					      `(let ((,branch-index
						      (row-major-aref ,(second sub-form) 0)))
						 (cond ,@(loop :for tag :in tags
							    :collect `((= ,branch-index ,(first tag))
								       (go ,(second tag))))))))))))
      (funcall (lambda (code) (if (not (assoc :compile-only options))
				  code `(quote ,code)))
	       (if (or system-vars vars-declared)
		   (funcall (lambda (workspace form)
			      (funcall (if (not workspace)
					   #'identity (lambda (form) `(in-apl-workspace ,workspace ,form)))
				       form))
			    (second (assoc :space options))
			    `(let* (,@system-vars ,@vars-declared)
			       (declare (ignorable ,@(mapcar #'first system-vars)
						   ,@(mapcar #'second var-symbols)))
			       ,@(if (not (gethash :branches meta))
				     exps `((let ((,tb-output))
					      (tagbody ,@(funcall
							  (lambda (list)
							    (append (butlast list 1)
								    `((setq ,tb-output
									    ,(first (last list))))))
							  (process-tags exps (gethash :branches meta))))
					      ,tb-output)))))
		   (if (< 1 (length exps))
		       `(progn ,@exps)
		       (first exps)))))))

(defun april-function-glyph-processor (type glyph spec)
  "Convert a Vex function specification for April into a set of lexicon elements, forms and functions that will make up part of the April idiom object used to compile the language."
  (let ((type (intern (string-upcase type) "KEYWORD"))
	(function-type (intern (string-upcase (first spec)) "KEYWORD"))
	(spec-body (rest spec)))
    (cond ((eq :symbolic function-type)
	   `(,glyph :lexicons (:functions :symbolic-functions)
		    :functions (:symbolic ,(first spec-body))))
	  ((keywordp (first spec-body))
	   ;; if this is a simple scalar declaration passing through another function
	   `(,glyph :lexicons (:functions :scalar-functions :monadic-functions :scalar-monadic-functions
					  ,@(if (not (eq :monadic function-type))
						(list :dyadic-functions :scalar-dyadic-functions)))
		    :functions ,(append (if (or (eq :ambivalent function-type)
						(eq :monadic function-type))
					    (list :monadic `(scalar-function ,(second spec-body))))
					(if (or (eq :ambivalent function-type)
						(eq :dyadic function-type))
					    (list :dyadic `(scalar-function ,(first (last spec-body))))))))
	  (t `(,glyph :lexicons ,(cond ((eq :functions type)
					`(:functions ,@(if (eq :ambivalent function-type)
							   '(:monadic-functions :dyadic-functions)
							   (list (intern (string-upcase
									  (concatenate 'string
										       (string function-type)
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
							   '(:scalar-functions :scalar-dyadic-functions))))
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
						     (if (eq :symbolic function-type)
							 (list :symbolic (first spec-body))))))
			      ((eq :operators type)
			       `(:operators ,(first spec-body)))))))))

;; a secondary package containing a set of tools for the extension of April idioms
(defpackage #:april.idiom-extension-tools
  (:import-from :april #:extend-vex-idiom #:april-function-glyph-processor #:scalar-function)
  (:export #:extend-vex-idiom #:april-function-glyph-processor #:scalar-function))

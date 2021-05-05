;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Vex -*-
;;;; vex.lisp

(in-package #:vex)

"A framework for building vector languages; its current use case is the implementation of the April dialect of APL."

;; The idiom object defines a vector language instance with glyph lexicons and a set of processing utilities.
(defclass idiom ()
  ((name :accessor idiom-name
    	 :initarg :name)
   (system :accessor idiom-system
	   :initform nil
	   :initarg :system)
   (symbols :accessor idiom-symbols
	    :initform nil
	    :initarg :symbols)
   (utilities :accessor idiom-utilities
	      :initform nil
	      :initarg :utilities)
   (lexicons :accessor idiom-lexicons
	     :initform nil
	     :initarg :lexicons)
   (functions :accessor idiom-functions
	      :initform nil
	      :initarg :functions)
   (operators :accessor idiom-operators
	      :initform nil
	      :initarg :operators)
   (grammar-elements :accessor idiom-grammar-elements
		     :initform nil
		     :initarg :grammar-elements)
   (composer-opening-patterns :accessor idiom-composer-opening-patterns
			      :initform nil
			      :initarg :composer-opening-patterns)
   (composer-following-patterns :accessor idiom-composer-following-patterns
				:initform nil
				:initarg :composer-following-patterns)))

(defmacro local-idiom (symbol)
  "Shorthand macro to output the name of a Vex idiom in the local package."
  (let ((sym (intern (format nil "*~a-IDIOM*" (string-upcase symbol))
		     (string-upcase symbol))))
    (if (boundp sym)
	sym `(progn (defvar ,sym) ,sym))))

(defgeneric get-system-meta (idiom property))
(defmethod get-system-meta ((idiom idiom) property)
  "Retrieve a property of the idiom's system."
  (getf (idiom-system idiom) property))

(defgeneric set-system-meta (idiom &rest pairs))
(defmethod set-system-meta ((idiom idiom) &rest pairs)
  (loop :for (key value) :on pairs :by #'cddr
     :do (setf (getf (idiom-system idiom) key) value)))

(defgeneric of-utilities (idiom utility))
(defmethod of-utilities ((idiom idiom) utility)
  "Retrieve one of the idiom's utilities used for parsing and language processing."
  (getf (idiom-utilities idiom) utility))

(defgeneric of-functions (idiom key type))
(defmethod of-functions ((idiom idiom) key type)
  "Retrive one of the idiom's functions."
  (let ((function-set (idiom-functions idiom)))
    (if type (gethash key (getf function-set type))
	(let ((output))
	  (loop :for (set-key value) :on function-set :by #'cddr :while (not output)
	     :do (if (gethash key value)
		     (setf output (gethash key value))))
	  output))))

(defgeneric of-operators (idiom key type))
(defmethod of-operators ((idiom idiom) key type)
  "Retrive one of the idiom's operators."
  (let ((operator-set (idiom-operators idiom)))
    (if type (gethash key (getf operator-set type))
	(let ((output))
	  (loop :for (set-key value) :on operator-set :by #'cddr :while (not output)
	     :do (if (gethash key value)
		     (setf output (gethash key value))))
	  output))))

(defgeneric of-lexicon (idiom lexicon glyph))
(defmethod of-lexicon ((idiom idiom) lexicon glyph)
  "Check whether a character belongs to a given Vex lexicon."
  (member glyph (getf (idiom-lexicons idiom) lexicon)))

(defmacro boolean-op (operation)
  "Wrap a boolean operation for use in a vector language, converting the t or nil it returns to 1 or 0."
  (let ((omega (gensym)) (alpha (gensym)) (outcome (gensym)))
    `(lambda (,omega &optional ,alpha)
       (let ((,outcome (funcall ,(if (symbolp operation)
				     `(function ,operation)
				     (macroexpand operation))
				,alpha ,omega)))
	 (if ,outcome 1 0)))))

(defmacro reverse-op (is-dyadic &optional operation)
  "Wrap a function so as to reverse the arguments passed to it and (- 5 10) will thus result in 5."
  (let ((is-dyadic (if operation is-dyadic))
	(operation (if operation operation is-dyadic))
	(omega (gensym)) (alpha (gensym)))
    `(lambda (,omega &optional ,alpha)
       ,(if is-dyadic `(funcall (function ,operation) ,alpha ,omega)
	    `(if ,alpha (funcall (function ,operation) ,alpha ,omega)
		 (funcall (function ,operation) ,omega))))))

(defun count-symbol-in-spec (symbol limbs)
  "Count occurrences of a symbol in the spec. Used to plan the number of Prove tests to run."
  (let ((results 0))
    (loop :for limb :in limbs :do (if (listp limb)
				      (incf results (count-symbol-in-spec symbol limb))
				      (if (and (symbolp limb)
					       (eql symbol limb))
					  (incf results))))
    results))

(defun process-lex-tests-for (symbol operator &key (mode :test))
  "Process a set of tests for Vex functions or operators."
  (let* ((tests (rest (assoc (intern "TESTS" (package-name *package*))
			     (rest operator))))
	 (props (rest (assoc (intern "HAS" (package-name *package*))
			     (rest operator))))
	 (heading (format nil "[~a] ~a~a~%" (first operator)
			  (if (getf props :title)
			      (getf props :title)
			      (if (getf props :titles)
				  (first (getf props :titles))))
			  (if (not (getf props :titles))
			      "" (concatenate 'string " / " (second (getf props :titles)))))))
    (labels ((for-tests (tests &optional output)
	       (if tests (for-tests (rest tests)
				    (append output (if (not (eq :time mode))
						       `((princ (format nil "  _ ~a" ,(cadr (first tests))))))
					    (cond ((and (eq :test mode)
							(eql 'is (caar tests)))
						   `((is (,(intern (string-upcase symbol)
								   (package-name *package*))
							   ,(cadar tests))
							 ,(third (first tests))
							 :test #'equalp)))
						  ((and (eq :time mode)
							(eql 'is (caar tests)))
						   `((,(intern (string-upcase symbol)
							       (package-name *package*))
						       ,(cadar tests))))
						  ((and (eq :demo mode)
							(eql 'is (caar tests)))
						   `((princ #\Newline)
						     (let ((output
							    (,(intern (string-upcase symbol)
								      (package-name *package*))
							      (with (:state :output-printed :only))
							      ,(cadar tests))))
						       (princ (concatenate
							       'string "    "
							       (regex-replace-all
								"[\\n]" output
								,(concatenate 'string '(#\Newline) "    "))))
						       (if (or (= 0 (length output))
							       (not (char= #\Newline
									   (aref output (1- (length
											     output))))))
							   (princ #\Newline)))
						     ,@(if (rest tests)
							   `((princ #\Newline))))))))
		   output)))
      (if tests (append (if (not (eq :time mode))
			    `((princ ,(format nil "~%~a" heading))))
			(for-tests tests))))))

;; TODO: this is also April-specific, move it into spec
(defun process-general-tests-for (symbol test-set &key (mode :test))
  "Process specs for general tests not associated with a specific function or operator."
  (append (if (not (eq :time mode))
	      `((princ ,(format nil "~%~a~a" (cond ((string= "FOR" (string-upcase (first test-set)))
						    "⍎ ")
						   ((string= "FOR-PRINTED" (string-upcase (first test-set)))
						    (if (eq :test mode) "⎕ Printed: " "⎕ ")))
				(second test-set)))
		(princ (format nil "~%  _ ~a~%" ,(third test-set)))))
	  (list (cond ((and (eq :test mode)
			    (string= "FOR" (string-upcase (first test-set))))
		       `(is (,(intern (string-upcase symbol) (package-name *package*))
			      ,(third test-set))
			    ,(fourth test-set)
			    :test #'equalp))
		      ((and (eq :time mode)
			    (string= "FOR" (string-upcase (first test-set))))
		       `(,(intern (string-upcase symbol) (package-name *package*))
			  ,(third test-set)))
		      ((and (eq :demo mode)
			    (string= "FOR" (string-upcase (first test-set))))
		       `(let ((output (,(intern (string-upcase symbol) (package-name *package*))
					(with (:state :output-printed :only))
					,(third test-set))))
			  (princ (concatenate 'string "    "
					      (regex-replace-all "[\\n]" output
								 ,(concatenate 'string '(#\Newline)  "    "))))
			  (if (or (= 0 (length output))
				  (not (char= #\Newline (aref output (1- (length output))))))
			      (princ #\Newline))))
		      ((and (eq :test mode)
			    (string= "FOR-PRINTED" (string-upcase (first test-set))))
		       `(is (,(intern (string-upcase symbol) (package-name *package*))
			      (with (:state :output-printed :only))
			      ,(third test-set))
			    ,(fourth test-set)
			    :test #'string=))
		      ((and (eq :time mode)
			    (string= "FOR-PRINTED" (string-upcase (first test-set))))
		       `(,(intern (string-upcase symbol) (package-name *package*))
			  (with (:state :output-printed :only))
			  ,(third test-set)))
		      ((and (eq :demo mode)
			    (string= "FOR-PRINTED" (string-upcase (first test-set))))
		       `(let ((output (,(intern (string-upcase symbol) (package-name *package*))
					(with (:state :output-printed :only))
					,(third test-set))))
			  (princ (concatenate 'string "    "
					      (regex-replace-all "[\\n]" output
								 ,(concatenate 'string '(#\Newline)  "    "))))
			  (if (or (= 0 (length output))
				  (not (char= #\Newline (aref output (1- (length output))))))
			      (princ #\Newline))))))))

(defun process-arbitrary-tests-for (symbol test-set &key (mode :test))
  "Process arbitrary tests within a spec containing expressions that are evaluated without being wrapped in an (april ...) form."
  (declare (ignore symbol mode))
  (loop :for test :in test-set :append (append '((princ (format nil "~%")))
					       (list test))))

(defmacro specify-vex-idiom (symbol &rest subspecs)
  "Wraps the idiom-spec macro for an initial specification of a Vex idiom."
  `(vex-idiom-spec ,symbol nil ,@subspecs))

(defmacro extend-vex-idiom (symbol &rest subspecs)
  "Wraps the idiom-spec macro for an extension of a Vex idiom."
  `(vex-idiom-spec ,symbol t ,@subspecs))

(defun merge-lexicons (source &optional target)
  "Combine two Vex symbol lexicons."
  (if (not source)
      target (merge-lexicons (cddr source)
			     (progn (setf (getf target (first source))
					  (append (getf target (first source))
						  (second source)))
				    target))))

(defun merge-options (source target)
  "Merge options from multiple Vex specifiction sections into a single set."
  (let ((output (loop :for section :in target
		   :collect (let ((source-items (rest (assoc (first section) source)))
				  (pair-index 0)
				  ;; the osection values are copied from (rest section), otherwise it's
				  ;; effectively a pass-by-reference and changing osection will change section
				  (osection (loop :for item :in (rest section) :collect item)))
			      (loop :for (item-name item-value) :on source-items :while item-value
				 :do (if (evenp pair-index)
					 (setf (getf osection item-name) item-value))
				 (incf pair-index))
			      (cons (first section)
				    osection)))))
    (loop :for section :in source :when (not (assoc (first section) target))
       :do (setq output (cons section output)))
    output))

(defun build-doc-profile (symbol spec mode section-names)
  "Build a documentation or test profile from a set of section names in a Vex idiom specification."
  (let ((specs (loop :for subspec :in spec
		  :when (or (string= "FUNCTIONS" (string-upcase (first subspec)))
			    (string= "OPERATORS" (string-upcase (first subspec)))
			    (string= "ARBITRARY-TEST-SET" (string-upcase (first subspec)))
			    (string= "TEST-SET" (string-upcase (first subspec))))
		  :collect subspec)))
    (loop :for name :in section-names
       :append (let* ((subspec (find name specs :test (lambda (id form)
							(eq id (second (assoc :name (rest (second form)))))))))
		 (append (cond ((eq :demo mode)
				`((princ (format nil "~%~%∘○( ~a~%  ( ~a~%"
						 ,(getf (rest (assoc :demo-profile (cdadr subspec)))
							:title)
						 ,(getf (rest (assoc :demo-profile (cdadr subspec)))
							:description)))))
			       ((eq :test mode)
				`((princ (format nil "~%~%∘○( ~a )○∘~%"
						 ,(getf (rest (assoc :tests-profile (cdadr subspec)))
							:title))))))
			 (loop :for test-set :in (cddr subspec)
			    :append (funcall (cond ((or (string= "FUNCTIONS" (string-upcase (first subspec)))
							(string= "OPERATORS" (string-upcase (first subspec))))
						    #'process-lex-tests-for)
						   ((string= "TEST-SET" (string-upcase (first subspec)))
						    #'process-general-tests-for)
						   ((string= "ARBITRARY-TEST-SET"
							     (string-upcase (first subspec)))
						    #'process-arbitrary-tests-for))
					     symbol test-set :mode mode)))))))

(defmacro vex-idiom-spec (symbol extension &rest subspecs)
  "Process the specification for a vector language and build functions that generate the code tree."
  (macrolet ((of-subspec (symbol-string)
	       `(rest (assoc ',symbol-string subspecs :test (lambda (x y) (string= (string-upcase x)
										   (string-upcase y))))))
	     (build-lexicon () `(loop :for lexicon :in (getf (rest this-lex) :lexicons)
				   :do (if (not (getf lexicon-data lexicon))
					   (setf (getf lexicon-data lexicon) nil))
				   (if (not (member char (getf lexicon-data lexicon)))
				       (setf (getf lexicon-data lexicon)
					     (cons char (getf lexicon-data lexicon)))))))
    (let* ((symbol-string (string-upcase symbol))
	   (idiom-symbol (intern (format nil "*~a-IDIOM*" symbol-string)
				 (symbol-package symbol)))
	   (lexicon-data)
	   (lexicon-processor (getf (of-subspec utilities) :process-lexicon))
	   (function-specs
	    (loop :for subspec :in subspecs :when (string= "FUNCTIONS" (string-upcase (first subspec)))
	       :append (loop :for spec :in (cddr subspec)
			  :append (let ((glyph-chars (cons (character (first spec))
							   (mapcar #'character (getf (rest (second spec))
										     :aliases))))
					(fn-params (third spec))
					(fn-params-inverse (if (string= "INVERSE"
									(string-upcase
									 (first (fourth spec))))
							       (second (fourth spec)))))
				    (loop :for char :in glyph-chars
				       :append (let ((this-lex (funcall (second lexicon-processor)
									:functions char fn-params
									fn-params-inverse)))
						 (build-lexicon)
						 `(,@(if (getf (getf (rest this-lex) :functions) :monadic)
							 `((gethash ,char (getf fn-specs :monadic))
							   ',(getf (getf (rest this-lex) :functions) :monadic)))
						     ,@(if (getf (getf (rest this-lex) :functions) :dyadic)
							   `((gethash ,char (getf fn-specs :dyadic))
							     ',(getf (getf (rest this-lex) :functions)
								     :dyadic)))
						     ,@(if (getf (getf (rest this-lex) :functions) :symbolic)
							   `((gethash ,char (getf fn-specs :symbolic))
							     ',(getf (getf (rest this-lex) :functions)
								     :symbolic)))
						     ,@(if (getf (getf (rest this-lex) :functions)
								 :monadic-inverse)
							   `((gethash ,char (getf fn-specs :monadic-inverse))
							     ',(getf (getf (rest this-lex) :functions)
								     :monadic-inverse)))
						     ,@(if (getf (getf (rest this-lex) :functions)
								 :dyadic-inverse)
							   `((gethash ,char (getf fn-specs :dyadic-inverse))
							     ',(getf (getf (rest this-lex) :functions)
								     :dyadic-inverse))))))))))
	   (operator-specs
	    (loop :for subspec :in subspecs :when (string= "OPERATORS" (string-upcase (first subspec)))
	       :append (loop :for spec :in (cddr subspec)
			  :append (let ((glyph-chars (cons (character (first spec))
							   (mapcar #'character (getf (rest (second spec))
										     :aliases)))))
				    (loop :for char :in glyph-chars
				       :append (let* ((this-lex (funcall (second lexicon-processor)
									 :operators char (third spec)))
						      (lexicons (getf (rest this-lex) :lexicons))
						      (this-key (cond ((member :lateral-operators lexicons)
								       :lateral)
								      ((member :pivotal-operators lexicons)
								       :pivotal)
								      ((member :unitary-operators lexicons)
								       :unitary))))
						 (build-lexicon)
						 `((gethash ,char (getf op-specs ,this-key))
						   ,(getf (rest this-lex) :operators))))))))
	   (demo-forms (build-doc-profile symbol subspecs :demo (rest (assoc :demo (of-subspec doc-profiles)))))
	   (test-forms (build-doc-profile symbol subspecs :test (rest (assoc :test (of-subspec doc-profiles)))))
	   (timed-forms (build-doc-profile symbol subspecs :time (rest (assoc :time (of-subspec doc-profiles)))))
	   (atest-forms (build-doc-profile symbol subspecs :test
					   (rest (assoc :arbitrary-test (of-subspec doc-profiles)))))
	   ;; note: the pattern specs are processed and appended in reverse order so that their ordering in the
	   ;; spec is intuitive, with more specific pattern sets such as optimization templates being included
	   ;; after less specific ones like the baseline grammar
	   (pattern-settings
	    `((idiom-composer-opening-patterns ,idiom-symbol)
	      (append (idiom-composer-opening-patterns ,idiom-symbol)
		      ,(second (assoc :opening-patterns (of-subspec grammar))))
	      (idiom-composer-following-patterns ,idiom-symbol)
	      (append (idiom-composer-following-patterns ,idiom-symbol)
		      ,(second (assoc :following-patterns (of-subspec grammar))))))
	   (idiom-definition `(make-instance 'idiom :name ,(intern symbol-string "KEYWORD")))
	   (printout-sym (concatenate 'string symbol-string "-F"))
	   (inline-sym (concatenate 'string symbol-string "-C"))
	   (elem (gensym)) (options (gensym)) (input-string (gensym)) (body (gensym)) (args (gensym))
	   (input-path (gensym)) (process (gensym)) (form (gensym)) (item (gensym)) (pathname (gensym))
	   (ws-name (gensym)) (ws-fullname (gensym)))
      `(progn ,@(if (not extension)
		    `((proclaim '(special ,idiom-symbol))
		      (setf (symbol-value (quote ,idiom-symbol)) ,idiom-definition)))
	      (setf (idiom-system ,idiom-symbol)
		    (append (idiom-system ,idiom-symbol)
			    ,(cons 'list (of-subspec system)))
		    (idiom-utilities ,idiom-symbol)
		    (append (idiom-utilities ,idiom-symbol)
			    ,(cons 'list (of-subspec utilities)))
		    (idiom-symbols ,idiom-symbol)
		    (append (idiom-symbols ,idiom-symbol)
			    ,(list 'quote (of-subspec symbols)))
		    (idiom-lexicons ,idiom-symbol)
		    (merge-lexicons (idiom-lexicons ,idiom-symbol)
				    (quote ,lexicon-data))
		    (idiom-functions ,idiom-symbol)
		    (let ((fn-specs ,(if extension `(idiom-functions ,idiom-symbol)
					 `(list :monadic (make-hash-table) :dyadic (make-hash-table)
						:symbolic (make-hash-table)
						:monadic-inverse (make-hash-table)
						:dyadic-inverse (make-hash-table)))))
		      (setf ,@function-specs)
		      fn-specs)
		    (idiom-operators ,idiom-symbol)
		    (let ((op-specs ,(if extension `(idiom-operators ,idiom-symbol)
					 `(list :lateral (make-hash-table) :pivotal (make-hash-table)
						:unitary (make-hash-table)))))
		      (setf ,@operator-specs)
		      op-specs))
	      ,@(if (assoc :elements (of-subspec grammar))
		    `((setf (idiom-grammar-elements ,idiom-symbol)
			    ,(second (assoc :elements (of-subspec grammar))))))
	      ;; ,@(if (assoc :opening-patterns2 (of-subspec grammar))
	      ;; 	    `((setf (idiom-grammar-elements ,idiom-symbol)
	      ;; 		    ,(second (assoc :opening-patterns2 (of-subspec grammar))))))
	      (setf ,@pattern-settings)
	      ,@(if (not extension)
		    `((defmacro ,(intern symbol-string (symbol-package symbol))
			  (,options &optional ,input-string)
			;; this macro is the point of contact between users and the language, used to
			;; evaluate expressions and control properties of the language instance
			(cond ((and ,options (listp ,options)
				    (string= "TEST" (string-upcase (first ,options))))
			       ;; the (test) setting is used to run tests
			       `(progn (setq prove:*enable-colors* nil)
				       (plan ,(+ (loop :for exp :in ',test-forms :counting (eql 'is (first exp)))
						 (count-symbol-in-spec 'prove:is ',atest-forms)))
				       (,',(intern (concatenate 'string "WITH-" symbol-string "-CONTEXT")
				        	   (symbol-package symbol))
					   ,,(getf (of-subspec utilities) :test-parameters)
					   ,@',test-forms ,@',atest-forms (finalize))
				       (setq prove:*enable-colors* t)))
			      ((and ,options (listp ,options)
				    (string= "TIME-TESTS" (string-upcase (first ,options))))
			       `(progn (time (progn ,@',timed-forms))
				       ,(format nil "Timed evaluation of ~d tests." (length ',timed-forms))))
			      ((and ,options (listp ,options)
				    (string= "DEMO" (string-upcase (first ,options))))
			       ;; the (demo) setting is used to print demos of the language
			       `(progn ,@',demo-forms "Demos complete!"))
			      (t (if (or (and ,input-string (or (stringp ,input-string)
								(listp ,input-string)))
					 (and (not ,input-string)
					      (stringp ,options)))
				     (vex-program ,idiom-symbol
						  (if ,input-string
						      (if (or (string= "WITH" (string (first ,options)))
							      (string= "SET" (string (first ,options))))
							  (rest ,options)
							  (error "Incorrect option syntax.")))
						  (if ,input-string ,input-string ,options))
				     ;; this clause results in compilation at runtime of an
				     ;; evaluated string value
				     `(eval (vex-program
					     ,',idiom-symbol
					     ,(if ,input-string
						  (if (or (string= "WITH" (string (first ,options)))
							  (string= "SET" (string (first ,options))))
						      `(quote ,(rest ,options))
						      (error "Incorrect option syntax.")))
					     ,(if ,input-string ,input-string ,options)))))))
		      (defmacro ,(intern printout-sym (symbol-package symbol))
			  (&rest ,options)
			;; an alternate evaluation macro that prints formatted evaluation results
			;; as well as returning them
			(cons ',(intern symbol-string (symbol-package symbol))
			      (append (if (second ,options)
					  (list (cons (caar ,options)
						      (merge-options `((:state :print t))
								     (cdar ,options))))
					  `((with (:state :print t))))
				      (last ,options))))
		      (defmacro ,(intern inline-sym (symbol-package symbol))
			  (,options &rest ,args)
			;; an alternate evaluation macro that calls a function on arguments passed inline;
			;; makes for more compact invocations of the language
			;; TODO: can this be made to work with code passed in string-referencing variables?
			(let ((,args (if (stringp ,options)
					 ,args (rest ,args)))
			      (,input-string (if (listp ,options)
						 (first ,args))))
			  (apply #'vex-program ,idiom-symbol
				 (if ,input-string
				     (if (or (string= "WITH" (string (first ,options)))
					     (string= "SET" (string (first ,options))))
					 (rest ,options)
					 (error "Incorrect option syntax.")))
				 (if ,input-string ,input-string ,options)
				 ,args)))
		      (defmacro ,(intern (concatenate 'string symbol-string "-LOAD")
					 (symbol-package symbol))
			  (,options &optional ,input-path)
			;; an evaluation macro that loads code from a file,
			;; evaluating the path expression
			`(progn ,(let ((,pathname (if ,input-path (eval ,input-path)
						      (eval ,options))))
				   (if (pathnamep ,pathname)
				       (vex-program ,idiom-symbol
						    (if ,input-path
							(if (or (string= "WITH" (string (first ,options)))
								(string= "SET" (string (first ,options))))
							    (rest ,options)
							    (error "Incorrect option syntax.")))
						    ,pathname)
				       (error "Argument to be loaded was not a pathname.")))))
		      (defmacro ,(intern (concatenate 'string "WITH-" symbol-string "-CONTEXT")
					 (symbol-package symbol))
			  (,options &rest ,body)
			;; this macro creates a context enclosure within which evaluations have a default
			;; context; use this to evaluate many times with the same (with) expression
			(labels ((,process (,form)
				   (loop :for ,item :in ,form
				      :collect (if (and (listp ,item)
							(or (eql ',(intern symbol-string (symbol-package symbol))
								 (first ,item))
							    (eql ',(intern (concatenate 'string symbol-string
											"-LOAD")
									   (symbol-package symbol))
								 (first ,item))
							    (eql ',(intern printout-sym (symbol-package symbol))
								 (first ,item))))
						   (list (first ,item)
							 (if (third ,item)
							     (cons (caadr ,item)
							 	   (merge-options (cdadr ,item)
										  ,options))
							     (cons 'with ,options))
							 (first (last ,item)))
						   (if (listp ,item)
						       (,process ,item)
						       ,item)))))
			  (cons 'progn (,process ,body))))
		      (defmacro ,(intern (concatenate 'string symbol-string "-CREATE-WORKSPACE")
					 (symbol-package symbol))
			  (,ws-name)
			;; this macro creates a context enclosure within which evaluations have a default
			;; context; use this to evaluate many times with the same (with) expression
			(let ((,ws-fullname (concatenate 'string ,(string-upcase symbol)
							"-WORKSPACE-" (string-upcase ,ws-name))))
			  `(if (not (find-package ,,ws-fullname))
			       (progn (make-package ,,ws-fullname)
			    	      (proclaim (list 'special (intern "*SYSTEM*" ,,ws-fullname)
			    			      (intern "*BRANCHES*" ,,ws-fullname)
			    			      (intern "*INDEX-ORIGIN*" ,,ws-fullname)
			    			      (intern "*PRINT-PRECISION*" ,,ws-fullname)
			    			      (intern "*COMPARISON-TOLERANCE*" ,,ws-fullname)))
			    	      (set (intern "*SYSTEM*" ,,ws-fullname) ,',(cons 'list (of-subspec system)))
				      ;; TODO: following is APL-specific, move into spec
			    	      (set (intern "*BRANCHES*" ,,ws-fullname) nil)
			    	      (set (intern "*INDEX-ORIGIN*" ,,ws-fullname)
			    	      	   ,,(getf (second (getf (of-subspec system) :workspace-defaults))
				      		   :index-origin))
			    	      (set (intern "*PRINT-PRECISION*" ,,ws-fullname)
			    	      	   ,,(getf (second (getf (of-subspec system) :workspace-defaults))
				      		   :print-precision))
			    	      (set (intern "*COMPARISON-TOLERANCE*" ,,ws-fullname)
			    	      	   ,,(getf (second (getf (of-subspec system) :workspace-defaults))
				      		   :comparison-tolerance))
				      (format nil "Successfully created workspace ｢~a｣." ',,ws-name))
			       (format nil "A workspace called ｢~a｣ already exists." ',,ws-name))))
		      (defmacro ,(intern (concatenate 'string symbol-string "-CLEAR-WORKSPACE")
					 (symbol-package symbol))
			  (,ws-name)
			;; this macro creates a context enclosure within which evaluations have a default
			;; context; use this to evaluate many times with the same (with) expression
			(let ((,ws-fullname (concatenate 'string ,(string-upcase symbol)
							 "-WORKSPACE-" (string-upcase ,ws-name))))
			  `(if (find-package ,,ws-fullname)
			       (progn (delete-package ,,ws-fullname)
			       	      (,',(intern (concatenate 'string symbol-string "-CREATE-WORKSPACE")
			       			(symbol-package symbol))
			       		 ,,ws-name)
			       	      ,(format nil "The workspace ｢~a｣ has been cleared." ,ws-name))
			       (progn (,',(intern (concatenate 'string symbol-string "-CREATE-WORKSPACE")
			       			  (symbol-package symbol))
			        	  ,,ws-name)
				      ,(format nil "No workspace called ｢~a｣ was found to clear; ~a"
					       ,ws-name "the workspace has been created..")))))))
	      ;; print a summary of the idiom as it was specified or extended
	      (let ((items 0)
		    (set-index 0)
		    (output "")
		    (sets (list (list "basic grammar element"
				      ,(if (not (assoc :elements (of-subspec grammar)))
					   0 (* 1/2 (length (cadadr (assoc :elements (of-subspec grammar)))))))
				(list "opening grammar pattern"
				      (+ ,@(loop :for pset :in (reverse (rest (assoc :opening-patterns
										     (of-subspec grammar))))
					      :collect `(length ,pset))))
				(list "following grammar pattern"
				      (+ ,@(loop :for pset :in (reverse (rest (assoc :following-patterns
										     (of-subspec grammar))))
					      :collect `(length ,pset))))
				(list "lexical function" ,(* 1/2 (length function-specs)))
				(list "lexical operator" ,(* 1/2 (length operator-specs)))
				(list "utility function" ,(* 1/2 (length (of-subspec utilities))))
				(list "unit test" ,(+ (loop :for exp :in test-forms
							 :counting (eql 'is (first exp)))
						      (count-symbol-in-spec 'prove:is atest-forms))))))
		(loop :for set-values :in sets
		   :do (destructuring-bind (set-name set) set-values
			 (setq output (if (= 0 set) output
					(format nil "~a~a~a ~a~a"
						output (if (and (< 0 items)
								(or (= set-index (1- (length sets)))
								    (= 0 (loop :for sx :from (1+ set-index)
									    :to (1- (length sets))
									    :summing (second (nth sx sets))))))
							   " and " (if (< 0 items) ", " ""))
						set set-name (if (< 1 set) "s" "")))
			       set-index (1+ set-index)
			       items (+ set items))))
		(princ (format nil "~%~a idiom ｢~a｣ with ~a.~%~%" ,(if extension "Extended" "Specified")
			       ,(string-upcase symbol)
			       output)))
	      ,(format nil "Idiom ~a complete." (if extension "extension" "specification"))))))

(defun derive-opglyphs (glyph-list &optional output)
  "Extract a list of function/operator glyphs from part of a Vex language specification."
  (if (not glyph-list)
      output (derive-opglyphs (rest glyph-list)
			      (let ((glyph (first glyph-list)))
				(if (characterp glyph)
				    (cons glyph output)
				    (if (stringp glyph)
					(append output (loop :for char :below (length glyph)
							  :collect (aref glyph char)))))))))

(defun =vex-string (idiom &optional output special-precedent)
  "Parse a string of text, converting its contents into nested lists of Vex tokens."
  (let ((string-found))
    (labels ((?blank-character () (?satisfies (of-utilities idiom :match-blank-character)))
	     (?newline-character () (?satisfies (of-utilities idiom :match-newline-character)))
	     (?token-character () (?satisfies (of-utilities idiom :match-token-character)))
	     (=string (&rest delimiters)
	       (let ((lastc) (delimiter) (escape-indices) (char-index 0))
		 (=destructure (_ content)
		     (=list (?satisfies (lambda (c) (if (member c delimiters)
							(setq delimiter c))))
			    ;; note: nested quotes must be checked backwards; to determine whether a delimiter
			    ;; indicates the end of the quote, look at previous character to see whether it is a
			    ;; delimiter, then check whether the current character is an escape character #\\
			    (=subseq (%any (?satisfies (lambda (char)
							 (if (or (not lastc)
								 (not (char= lastc delimiter))
								 (char= char delimiter))
							     (setq lastc (if (and lastc (char= char delimiter)
										  (or (char= lastc delimiter)
										      (char= lastc #\\)))
									     (progn (setq escape-indices
											  (cons (1- char-index)
												escape-indices))
										    #\ )
									     char)
								   char-index (1+ char-index))))))))
		   (setq string-found t)
		   ;; the string-found variable is set to true
		   ;; TODO: is there a better way to do this?
		   (if escape-indices (let* ((offset 0)
					     (outstr (make-array (list (- (length content)
									  1 (length escape-indices)))
								 :element-type 'character)))
					(loop :for x :below (1- (length content))
					   :when (member x escape-indices) :do (incf offset)
					   :when (not (member x escape-indices))
					   :do (setf (aref outstr (- x offset)) (aref content x)))
					outstr)
		       (if (= 2 (length content))
			   (aref content 0)
			   (make-array (1- (length content)) :element-type 'character
				       :displaced-to content))))))
	     (=vex-closure (boundary-chars &optional transform-by
					   &key (disallow-linebreaks) (symbol-collector))
	       (let* ((balance 1)
		      (char-index 0)
		      ;; disallow linebreak overriding opening and closing characters
		      (dllen (if (stringp disallow-linebreaks) (length disallow-linebreaks)))
		      (dlbor-opening-chars (if dllen (subseq disallow-linebreaks 0 (/ dllen 2))))
		      (dlbor-closing-chars (if dllen (subseq disallow-linebreaks (/ dllen 2) dllen)))
		      (dlb-overriding-balance 0))
		 (=destructure (_ enclosed _)
		     (=list (?eq (aref boundary-chars 0))
			    ;; for some reason, the first character in the string is iterated over twice here,
			    ;; so the character index is checked and nothing is done for the first character
			    ;; TODO: fix this
			    (=transform (=subseq
					 (%some (?satisfies
						 (lambda (char)
						   (if (and disallow-linebreaks
							    (if (funcall (of-utilities
									  idiom :match-newline-character)
									 char)
								(or (= 0 dlb-overriding-balance)
								    (not (funcall
									  (of-utilities
									   idiom
									   :match-inline-newline-character)
									  char)))))
						       (error "Newlines cannot occur within a ~a closure."
							      boundary-chars))
						   (if (and (char= char (aref boundary-chars 0))
							    (< 0 char-index))
						       (incf balance)
						       (if (and (char= char (aref boundary-chars 1))
								(< 0 char-index))
							   (decf balance)))
						   (if dlbor-opening-chars
						       (if (not (loop :for c :across dlbor-opening-chars
								   :never (char= char c)))
							   (incf dlb-overriding-balance)
							   (if (not (loop :for c :across dlbor-closing-chars
								       :never (char= char c)))
							       (decf dlb-overriding-balance))))
						   (incf char-index 1)
						   (< 0 balance)))))
					(if transform-by transform-by
					    (lambda (string-content)
					      (destructuring-bind (parsed remaining meta)
						  (parse string-content (=vex-string idiom))
						(if symbol-collector (funcall symbol-collector meta))
						parsed))))
			    (%any (?eq (aref boundary-chars 1))))
		   (if (= 0 balance)
		       enclosed (error "No closing ~a found for opening ~a."
				       (aref boundary-chars 1) (aref boundary-chars 0))))))
	     (=vex-errant-closing-character (boundary-chars)
	       (let ((errant-char) (matching-char)
		     (chars-count (/ (length boundary-chars) 2)))
		 (=destructure (_ rest)
		     (=list (?satisfies (lambda (char)
					  (loop :for i :across boundary-chars
					     :for x :from 0 :to chars-count :when (char= char i)
					     :do (setq errant-char i
						       matching-char (aref boundary-chars (+ x chars-count))))
					  errant-char))
			    (=subseq (%any (?satisfies 'characterp))))
		   (error "Mismatched enclosing characters; each closing ~a must be preceded by an opening ~a."
			  errant-char matching-char))))
	     (process-lines (lines &optional output meta)
	       (if (or (= 0 (length lines))
		       (loop :for c :across lines :always (char= c #\ )))
		   (list output meta)
		   (destructuring-bind (out remaining meta)
		       (parse lines (=vex-string idiom nil meta))
		     (process-lines remaining (append output (if out (list out)))
				    meta))))
	     (handle-axes (symbol-collector)
	       (lambda (input-string)
		 (let* ((each-axis (funcall (of-utilities idiom :process-axis-string)
					    input-string))
			(each-axis-code (loop :for axis :in each-axis :collect
					     (let ((output (process-lines axis)))
					       (funcall symbol-collector (second output))
					       (first output)))))
		   (cons :axes each-axis-code))))
	     (handle-function (input-string)
	       (destructuring-bind (content meta) (process-lines input-string)
	     	 (let* ((is-operator (intersection '("⍺⍺" "⍵⍵") (getf meta :symbols)
						   :test (lambda (a b) (string= a (string b)))))
			(is-operator-pivotal (member "⍵⍵" is-operator :test #'string=)))
	     	   (if (and is-operator (not (getf meta :valence)))
	     	       (setf (getf meta :valence) (if is-operator-pivotal :pivotal :lateral)))
		   ;; (print (list :mt meta))
	     	   (list (if is-operator :op :fn)
	     		 meta content)))))

      (let ((olnchar)
	    (symbols))
	;; the olnchar variable is needed to handle characters that may be functional or part
	;; of a number based on their context; in APL it's the . character, which may begin a number like .5
	;; or may work as the inner/outer product operator, as in 1 2 3+.×4 5 6.
	(symbol-macrolet ((functional-character-matcher
			   ;; this saves space below
			   (let ((ix 0))
			     (lambda (char)
			       (if (and (> 2 ix)
					(funcall (of-utilities idiom :match-overloaded-numeric-character) char))
				   (setq olnchar char))
			       (if (and olnchar (= 2 ix)
					(not (digit-char-p char)))
				   (setq olnchar nil))
			       (incf ix 1)
			       (and (not (< 2 ix))
				    (or (of-lexicon idiom :functions char)
					(of-lexicon idiom :operators char)))))))
	  (=destructure (_ item _ break rest)
	      (=list (%any (?blank-character))
		     (%or (=vex-closure "()" nil :disallow-linebreaks "{}"
					:symbol-collector
					(lambda (meta) (setf symbols (append symbols (getf meta :symbols)))))
			  (=vex-closure "[]" (handle-axes
					      (lambda (meta) (setf symbols
								   (append symbols (getf meta :symbols))))))
			  (=vex-closure "{}" #'handle-function)
			  (=vex-errant-closing-character ")]}([{")
			  (=string #\' #\")
			  (=transform (=subseq (%some (?satisfies functional-character-matcher)))
				      (lambda (string)
					(let ((char (character string)))
					  (if (not olnchar)
					      (append (list (if (of-lexicon idiom :operators char)
								:op (if (of-lexicon idiom :functions char)
									:fn)))
						      (if (of-lexicon idiom :operators char)
							  (list (if (of-lexicon idiom :pivotal-operators char)
								    :pivotal
								    (if (of-lexicon idiom
										    :lateral-operators char)
									:lateral :unitary))))
						      (list char))))))
			  (=transform (=subseq (%some (?token-character)))
				      (lambda (string)
					(multiple-value-bind (formatted is-symbol)
					    (funcall (of-utilities idiom :format-value)
						     (string-upcase (idiom-name idiom))
						     ;; if there's an overloaded token character passed in
						     ;; the special precedent, prepend it to the token
						     ;; being processed
						     (idiom-symbols idiom)
						     (if (getf special-precedent :overloaded-num-char)
							 (format nil "~a~a"
						 		 (getf special-precedent :overloaded-num-char)
						 		 string)
							 string))
					  (if is-symbol (setq symbols (cons formatted symbols)))
					  formatted)))
			  ;; this last clause returns the remainder of the input in case the input has either no
			  ;; characters or only blank characters before the first line break
			  (=subseq (%any (?satisfies 'characterp))))
		     (%any (?blank-character))
		     (=subseq (%any (?newline-character)))
		     (=subseq (%any (?satisfies 'characterp))))
	    ;; (print (list :sym symbols item special-precedent))
	    (if (or symbols (member :symbols special-precedent))
		(setf (getf special-precedent :symbols)
		      (append symbols (getf special-precedent :symbols))))
	    (if (and (not output) (stringp item) (< 0 (length item))
		     (funcall (of-utilities idiom :match-newline-character)
			      (aref item 0)))
		;; if the string is passed back (minus any leading whitespace) because the string began with
		;; a line break, parse again omitting the line break character
		(parse (subseq item 1) (=vex-string idiom nil special-precedent))
		(if (and (= 0 (length break))
			 (< 0 (length rest)))
		    (parse rest (=vex-string idiom (if output (if (not item) output (cons item output))
						       (if item (list item)))
					     (append (if olnchar (list :overloaded-num-char olnchar))
						     (list :symbols (getf special-precedent :symbols)))))
		    (list (if (or (not item)
				  (and (typep item 'sequence)
				       (= 0 (length item)) (not string-found)))
			      ;; return nothing if only an empty sequence results from parsing
			      ;; unless an explicit empty string was parsed
			      output (cons item output))
			  rest special-precedent)))))))))

(defmacro set-composer-elements (name with &rest params)
  "Specify basic language elements for a Vex composer."
  (let* ((with (rest with))
	 (tokens (getf with :tokens-symbol))
	 (idiom (getf with :idiom-symbol))
	 (space (getf with :space-symbol with))
	 (properties (getf with :properties-symbol))
	 (pre-props (getf with :preprops-symbol))
	 (process (getf with :processor-symbol with)))
    `(defun ,(intern (string-upcase name) (package-name *package*)) (,idiom)
       (declare (ignorable ,idiom))
       (list ,@(loop :for param :in params
		  :collect `(list ,(intern (string-upcase (first param)) "KEYWORD")
				  (lambda (,tokens &optional ,properties ,process ,idiom ,space ,pre-props)
				    (declare (ignorable ,properties ,process ,idiom ,space ,pre-props))
				    ,(second param))))))))

(defun composer (idiom space tokens &optional precedent properties pre-props)
  "Compile processed tokens output by the parser into code according to an idiom's grammars and primitive elements."
  (if (not tokens)
      (values precedent properties)
      (let ((processed)
	    (pre-props (if (not properties)
			   pre-props (cons properties pre-props)))
	    (special-params (getf properties :special)))
	;; previous property values are stored in preceding-props so that grammar elements that
	;; need to know the properties of precedents can access them for pivotal and train compositions
	;; (print (list :prec pre-props precedent tokens properties))
	(loop :while (not processed)
	   :for pattern :in (if precedent (idiom-composer-following-patterns idiom)
				(idiom-composer-opening-patterns idiom))
	   :when (or (not (getf special-params :omit))
		     (not (member (getf pattern :name) (getf special-params :omit))))
	   :do (multiple-value-bind (new-processed new-props remaining)
		   (funcall (symbol-function (getf pattern :function))
			    tokens space idiom (lambda (item &optional sub-props)
						 (composer idiom space item nil sub-props))
			    precedent properties pre-props)
		 ;; (print (list :pattern (getf pattern :name) precedent tokens properties))
	       (if new-processed (setq processed new-processed properties new-props tokens remaining))))
	(if special-params (setf (getf properties :special) special-params))
	(if processed (composer idiom space tokens processed properties pre-props)
	    (values precedent properties tokens)))))

(defmacro composer-pattern-template
    (macro-names tokens-sym space-sym idiom-sym process-sym precedent-sym
     properties-sym preceding-properties-sym special-props-sym items-sym item-sym rest-items-sym)
  (let ((name (gensym)) (var-names (gensym))
	(assignment-clauses (gensym)) (generation (gensym)) (out-form (gensym)) (out-props (gensym))
	(axis (gensym)) (subax (gensym)) (symbol (gensym)) (symbol-form (gensym)) (symbol-props (gensym))
	(function (gensym)) (prec-spec (gensym)) (form-out (gensym))
	(form-properties (gensym)) (remaining (gensym))
	(args-list (list tokens-sym space-sym idiom-sym process-sym '&optional precedent-sym properties-sym
			 preceding-properties-sym items-sym item-sym rest-items-sym)))
    `(progn (defmacro ,(first macro-names) (,name ,var-names ,assignment-clauses &body ,generation)
	      `(defun ,,name ,',args-list
       		 (declare (ignorable ,',precedent-sym ,',properties-sym ,',preceding-properties-sym))
		 (symbol-macrolet ((,',item-sym (first ,',items-sym)) (,',rest-items-sym (rest ,',items-sym)))
       		   (let* ((,',items-sym ,',tokens-sym)
			  (,',special-props-sym (getf (first ,',preceding-properties-sym) :special))
			  ,@(mapcar #'list ,var-names))
       		     ,@,assignment-clauses
		     (if (not (member ,(intern (string-upcase ,name) "KEYWORD")
				      (getf ,',special-props-sym :omit)))
       			 (multiple-value-bind (,',out-form ,',out-props) ,(first ,generation)
       			   (if ,',out-form (values ,',out-form ,',out-props ,',items-sym)
       			       (values nil nil ,',tokens-sym)))
			 (values nil nil ,',tokens-sym))))))
	    (defmacro ,(second macro-names) (,symbol)
	      `(if (and (listp ,',item-sym) (eql :axes (first ,',item-sym)))
		   (setq ,,symbol (list (loop :for ,',axis :in (rest ,',item-sym)
					   :collect (funcall (lambda (item)
							        (if (< 1 (length item))
							      	   (cons 'progn item)
							      	   (first item)))
							     (loop :for ,',subax :in ,',axis
								:collect (funcall ,',process-sym ,',subax)))))
    			 ,',items-sym ,',rest-items-sym)))
	    (defmacro ,(third macro-names) (,symbol-form ,symbol-props ,function &optional ,properties-sym)
	      `(multiple-value-bind (,',form-out ,',form-properties)
		   (,,function ,',item-sym ,,properties-sym ,',process-sym ,',idiom-sym ,',space-sym)
		 (if ,',form-out (setq ,,symbol-form ,',form-out ,,symbol-props ,',form-properties
				       ,',items-sym ,',rest-items-sym))))
	    (defmacro ,(fourth macro-names) (,symbol-form ,symbol-props ,properties-sym)
	      `(multiple-value-bind (,',out-form ,',out-props ,',remaining)
		   (funcall ,',process-sym ,',items-sym ,,properties-sym)
		 (setq ,,symbol-form ,',out-form ,,symbol-props ,',out-props ,',items-sym ,',remaining))))))

#|
These are examples of the output of the three macro-builders above.

(defmacro assign-axes (symbol process item items rest-items)
  (let ((axis (gensym)))
    `(if (and (listp ,item) (eql :axes (first ,item)))
	 (setq ,symbol (list (loop :for ,axis :in (rest ,item) :collect (funcall ,process ,axis)))
    	       ,items ,rest-items))))

(defmacro assign-element (symbol-form symbol-props function process properties space item items rest-items)
  (let ((form-out (gensym)) (form-properties (gensym)))
    `(multiple-value-bind (,form-out ,form-properties)
	 (,function ,item ,properties ,process idiom ,space)
       (if ,form-out (setq ,symbol-form ,form-out ,symbol-props ,form-properties ,items ,rest-items)))))

(defmacro assign-subprocessed (symbol-form symbol-props process properties item items rest-items)
  (let ((form-out (gensym)) (form-properties (gensym)) (remaining (gensym)))
    `(multiple-value-bind (,form-out ,form-properties ,remaining)
	 (funcall ,process ,items ,properties)
       (setq ,symbol-form ,form-out ,symbol-props ,form-properties ,items ,remaining))))

|#

(defmacro ws-assign-val (symbol value)
  "Assignment macro for use with (:store-val) directive."
  `(progn (if (not (boundp ',symbol))
	      (proclaim '(special ,symbol)))
	  (setf (symbol-value ',symbol) ,value)))

(defmacro ws-assign-fun (symbol value)
  "Assignment macro for use with (:store-fun) directive."
  `(progn (if (not (boundp ',symbol))
	      (proclaim '(special ,symbol)))
	  (setf (symbol-function ',symbol) ,value)))

(defun vex-program (idiom options &optional string &rest inline-arguments)
  "Compile a set of expressions, optionally drawing external variables into the program and setting configuration parameters for the system."
  (let* ((state (rest (assoc :state options)))
	 (space (concatenate 'string (string-upcase (idiom-name idiom))
			    "-WORKSPACE-" (if (not (second (assoc :space options)))
					      "COMMON" (string-upcase (second (assoc :space options))))))
	 (state-to-use) (system-to-use))
    (labels ((assign-from (source dest)
	       (if (not source)
		   dest (progn (setf (getf dest (first source)) (second source))
			       (assign-from (cddr source) dest))))
	     (validate-var-symbol (symbol)
	       (let ((string-sym (if (stringp symbol) symbol (lisp->camel-case symbol))))
		 (loop :for c :across string-sym
		    :always (funcall (of-utilities idiom :match-token-character) c))))
	     (process-lines (lines &optional output)
	       (if (= 0 (length lines))
		   output (destructuring-bind (out remaining meta)
			      (parse lines (=vex-string idiom))
			    (let ((out (funcall (or (of-utilities idiom :lexer-postprocess)
						    (lambda (a b c) a))
						out idiom space)
				    ;; out
				    ))
			      (process-lines remaining
					     (if (null out)
						 output (append output
								(list (composer idiom space out nil nil
										'((:special
										   (:top-level t))))))))))))
	     (get-item-refs (items-to-store &optional storing-functions)
	       ;; Function or variable names passed as a string may be assigned literally as long as there are
	       ;; no dashes present in them, so the variable name "iD" becomes iD within the idiom, whereas a
	       ;; variable named |iD| will become id within the idiom. Strings are used instead of pipe-quoting
	       ;; because there's no way to tell the difference between symbols ABC and |ABC| after they
	       ;; pass the reader and the uppercase symbol names are converted to lowercase by default.
	       (loop :for item :in items-to-store
		  :collect (list (if storing-functions 'ws-assign-fun 'ws-assign-val)
				 (if (validate-var-symbol (first item))
				     (let ((symbol (if (and (stringp (first item))
							    (loop :for c :across (first item)
							       :never (char= #\- c)))
						       (string (first item))
						       (lisp->camel-case (first item)))))
				       ;; if functions or variables are to be stored in their workspace,
				       ;; initialize their values so compilation will proceed correctly
				       (funcall (of-utilities idiom :process-stored-symbol)
						symbol space storing-functions)
				       (intern symbol))
				     (error "Invalid characters present in symbol ~a passed to :~a."
					    (first item) (if storing-functions :store-fun :store-val)))
				 (second item)))))

      (symbol-macrolet ((ws-system (symbol-value (intern "*SYSTEM*" space))))
	
	(setq state (funcall (of-utilities idiom :preprocess-state-input) state)
	      state-to-use (assign-from (getf ws-system :base-state) state-to-use)
	      state-to-use (assign-from (getf ws-system :state) state-to-use)
	      state-to-use (assign-from state state-to-use)
	      system-to-use (assign-from ws-system system-to-use)
	      system-to-use (assign-from state system-to-use))

	(if string
	    (let* ((string (if (stringp string)
			       ;; just pass the string through if it's not a pathname;
			       ;; if it is a pathname, evaluate it in case something like
			       ;; (asdf:system-relative-pathname ...) was passed
			       string (with-open-file (stream (eval string))
					(apply #'concatenate
					       (cons 'string (loop :for line := (read-line stream nil)
								:while line
								:append (list line '(#\Newline))))))))
		   (input-vars (getf state-to-use :in))
		   (output-vars (getf state-to-use :out))
		   (stored-refs (append (get-item-refs (rest (assoc :store-val options)))
					(get-item-refs (rest (assoc :store-fun options)) t)))
		   (compiled-expressions (process-lines (funcall (of-utilities idiom :prep-code-string)
								 string)))
		   (system-vars (funcall (of-utilities idiom :system-lexical-environment-interface)
					 state-to-use))
		   (vars-declared (funcall (of-utilities idiom :build-variable-declarations)
					   input-vars space)))
	      (funcall (of-utilities idiom :build-compiled-code)
		       (append (funcall (if output-vars #'values
					    (apply (of-utilities idiom :postprocess-compiled)
						   system-to-use inline-arguments))
					compiled-expressions)
			       ;; if multiple values are to be output, add the (values) form at bottom
			       (if output-vars
				   (list (cons 'values (mapcar (lambda (return-var)
								 (intern (lisp->camel-case return-var) space))
							       output-vars)))))
		       (loop :for (key value) :on (getf (idiom-system idiom) :workspace-defaults)
			    :by #'cddr :collect (string-upcase key))
		       options system-vars vars-declared stored-refs space)))))))

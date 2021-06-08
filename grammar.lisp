;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; grammar.lisp

(in-package #:april)

"This file contains the specification of April's basic grammar elements, including the basic language components - array, function and operator - and the patterns comprising those elements that make up the language's strucures."

(define-symbol-macro include-lexvar-symbols
    (if (getf (getf (first (last preceding-properties)) :special) :lexvar-symbols)
	`(:lexvar-symbols ,(getf (getf (first (last preceding-properties)) :special)
				 :lexvar-symbols))))

(defun process-value (this-item properties process idiom space)
  ;; TODO: add a passthrough value mode for symbols that are being assigned!
  ;; this is the only way to get them assignable after the first time
  (cond ((and (listp this-item)
	      (not (member (first this-item) '(:fn :op :axes))))
	 ;; if the item is a closure, evaluate it and return the result
	 (multiple-value-bind (output out-properties)
	     (funcall process this-item)
	   (if (eq :array (first (getf out-properties :type)))
	       (progn (if (not (member :enclosed (getf out-properties :type)))
			  (setf (getf out-properties :type)
				(cons (first (getf out-properties :type))
				      (cons :enclosed (rest (getf out-properties :type))))))
		      (values output out-properties))
	       (values nil nil))))
	;; process the empty vector expressed by the [⍬ zilde] character
	((eq :empty-array this-item)
	 (values (make-array 0) '(:type (:array :empty))))
	;; process numerical values
	((and (numberp this-item)
	      (or (not (getf properties :type))
		  (eq :number (first (getf properties :type)))))
	 (values this-item '(:type (:array :number))))
	;; process string values
	((and (stringp this-item)
	      (or (not (getf properties :type))
		  (eq :string (first (getf properties :type)))))
	 (values this-item '(:type (:array :string))))
	;; process scalar character values
	((and (characterp this-item)
	      (or (not (getf properties :type))
		  (eq :character (first (getf properties :type)))))
	 (values this-item '(:type (:array :character))))
	;; process symbol-referenced values
	((and (symbolp this-item)
	      (or (member this-item '(⍵ ⍺) :test #'eql)
		  (getf properties :symbol-overriding)
		  (not (is-workspace-function this-item)))
	      (or (not (is-workspace-operator this-item))
		  (getf properties :symbol-overriding))
	      (not (member (intern (string-upcase this-item) *package-name-string*)
			   (rest (assoc :function (idiom-symbols idiom)))))
	      (not (member this-item '(⍺⍺ ⍵⍵ ∇ ∇∇) :test #'eql))
	      (or (not (getf properties :type))
		  (eq :symbol (first (getf properties :type)))))
	 (values (if (not (member (intern (string-upcase this-item) *package-name-string*)
				  (rest (assoc :function (idiom-symbols idiom)))))
		     this-item (intern (string-upcase this-item)))
		 '(:type (:symbol))))
	(t (values nil nil))))

(defun process-function (this-item properties process idiom space)
  (if (listp this-item)
      ;; process a function specification starting with :fn
      (if (or (eq :fn (first this-item))
	      ;; if marked as an operator, check whether the character is one entered as both
	      ;; a function and an operator; such functions must be dyadic
	      (and (eq :op (first this-item))
		   (or (of-lexicon idiom :dyadic-functions (third this-item))
		       (of-lexicon idiom :symbolic-functions (third this-item)))))
	  (let ((fn (first (last this-item)))
		(obligate-dyadic (and (eq :op (first this-item))
				      (of-lexicon idiom :dyadic-functions (third this-item))))
		(overloaded-operator (and (eq :op (first this-item))
					  (or (of-lexicon idiom :dyadic-functions (third this-item))
					      (of-lexicon idiom :symbolic-functions (third this-item))))))
	    (cond ((and (characterp fn)
			(or (not (getf properties :glyph))
			    (and (char= fn (aref (string (getf properties :glyph)) 0)))))
		   (values fn (list :type (append '(:function :glyph)
						  (if overloaded-operator '(:overloaded-operator))
						  (if (of-lexicon idiom :symbolic-functions (third this-item))
						      '(:symbolic-function))))))
		  ((and (listp fn)
			(not (getf properties :glyph)))
		   (let* ((polyadic-args (if (and (listp (first (last (first fn))))
						  (eq :axes (caar (last (first fn)))))
					     (mapcar #'caar (cdar (last (first fn))))))
			  (fn (if (not polyadic-args)
				  fn (cons (butlast (first fn) 1)
					   (rest fn))))
			  (initial-expr (first (last (first (last this-item)))))
			  (multi-args (if (and (listp (first (last initial-expr)))
					       (eq :axes (caar (last initial-expr))))
					  (mapcar #'caar (cdar (last initial-expr)))))
			  (assigned-symbols (get-assigned-symbols fn space))
			  (arg-symbols (intersection '(⍺ ⍵ ⍺⍺ ⍵⍵ ∇∇) (getf (second this-item) :symbols)))
			  (is-inline-operator (intersection arg-symbols '(⍺⍺ ⍵⍵ ∇∇))))
		     ;; if this is an inline operator, pass just that keyword back
		     (if is-inline-operator :is-inline-operator
			 (values (output-function
				  (mapcar (lambda (f)
					    (funcall process f
						     (list :special
							   (list :lexvar-symbols
								 (remove-duplicates
								  (append assigned-symbols
									  (getf (getf properties :special)
										:lexvar-symbols)))))))
					  fn)
				  polyadic-args assigned-symbols arg-symbols)
				 (list :type '(:function :closure)
				       :obligate-dyadic obligate-dyadic)))))
		  (t (values nil nil))))
	  ;; process sub-list in case it is a functional expression like (+∘*),
	  ;; but don't do this if looking for a specific functional glyph
	  (if (not (getf properties :glyph))
	      (multiple-value-bind (output out-properties)
	    	  (funcall process this-item)
	    	(if (eq :function (first (getf out-properties :type)))
	    	    (progn (if (not (member :enclosed (getf out-properties :type)))
			       (setf (getf out-properties :type)
	    			     (cons (first (getf out-properties :type))
	    				   (cons :enclosed (rest (getf out-properties :type))))))
	    		   (values output out-properties))
	    	    (values nil nil)))
	      (values nil nil)))
      (if (and (symbolp this-item)
	       (not (getf properties :glyph)))
	  (cond ((or (is-workspace-function this-item)
		     (get-workspace-alias space this-item))
		 ;; process workspace-aliased lexical functions, as when f←+ has been set
		 (values (if (not (get-workspace-alias space this-item))
			     this-item (get-workspace-alias space this-item))
			 (list :type '(:function :referenced))))
		((eql this-item '∇)
		 (values this-item (list :type '(:function :self-reference))))
		((member this-item '(⍵⍵ ⍺⍺))
		 (values (list 'ifn this-item)
			 (list :type '(:function :operator-reference))))
		((member (intern (string-upcase this-item) *package-name-string*)
			 (rest (assoc :function (idiom-symbols idiom))))
		 (values (list 'function (getf (rest (assoc :function (idiom-symbols idiom)))
					       (intern (string-upcase this-item)
						       *package-name-string*)))
			 (list :type '(:function :referenced))))
		(t (values nil nil)))
	  (values nil nil))))

(defun process-operator (this-item properties process idiom space)
  (if (listp this-item)
      (if (and (eq :op (first this-item))
	       (not (listp (first (last this-item))))
	       (or (not (getf properties :glyph))
		   (not (characterp (first (last this-item))))
		   (char= (character (getf properties :glyph))
			  (first (last this-item)))))
	  ;; process an operator token, allowing specification of the valence,
	  ;; either :lateral or :pivotal
	  (destructuring-bind (op-type op-symbol)
	      (rest this-item)
	    (let ((valid-by-valence (or (not (getf properties :valence))
					(eq op-type (getf properties :valence)))))
	      (if (and valid-by-valence (eql '∇∇ op-symbol))
		  (values :operator-self-reference
			  (list :type (list :operator op-type)))
		  (cond ((and valid-by-valence (getf properties :glyph))
			 (if (char= op-symbol (aref (string (getf properties :glyph)) 0))
			     (values op-symbol (list :type (list :operator op-type)))
			     (values nil nil)))
			(valid-by-valence (values op-symbol (list :type (list :operator op-type))))
			(t (values nil nil))))))
	  (if (and (eql :op (first this-item))
		   (listp (first (last this-item))))
	      (let* ((fn (first (last this-item)))
		     (assigned-symbols (get-assigned-symbols fn space))
		     (arg-symbols (intersection '(⍺ ⍵ ⍺⍺ ⍵⍵ ∇∇) (getf (second this-item) :symbols)))
		     (is-inline (intersection arg-symbols '(⍺⍺ ⍵⍵)))
		     (is-pivotal (member '⍵⍵ arg-symbols))
		     (valence (getf properties :valence)))
		(if is-inline (if (or (not valence)
				      (and is-pivotal (eq :pivotal valence))
				      (and (not is-pivotal) (eq :lateral valence)))
				  (values (output-function (mapcar process fn)
							   nil assigned-symbols arg-symbols)
					  (list :type (list :operator :closure
							    (if is-pivotal :pivotal :lateral)))))
		    (values nil nil)))
	      (values nil nil)))
      (if (symbolp this-item)
	  ;; if the operator is represented by a symbol, it is a user-defined operator
	  ;; and the appropriate variable name should be verified in the workspace
	  (let* ((symbol-string (string this-item))
		 (type-to-find (getf properties :valence))
		 (lop-string (if (eq :lateral (getf properties :valence))
				 (concatenate 'string "𝕆𝕃∇" symbol-string)))
		 (pop-string (if (eq :pivotal (getf properties :valence))
				 (concatenate 'string "𝕆ℙ∇" symbol-string)))
		 (bound-op (if (and lop-string (fboundp (intern lop-string space)))
			       (intern lop-string)
			       (if (and pop-string (fboundp (intern pop-string space)))
				   (intern pop-string)))))
	    (if bound-op
		(values bound-op (list :type (list :operator (or (getf properties :valence)
								 (if (fboundp (intern pop-string space))
								     :pivotal :lateral)))))
		(values nil nil)))
	  (values nil nil))))

(defun composer-pattern-value (tokens space idiom process &optional precedent properties preceding-properties)
  "Match an array like 1 2 3, marking the beginning of an array expression, or a functional expression if the array is an operand to a pivotal operator."
  (declare (ignorable precedent properties preceding-properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((axes) (value-elements) (value-props) (stopped) (items tokens))
      (labels ((axes-enclose (item axes)
		 (if (not axes) item (enclose-axes item axes))))
	(progn (if (and (listp item) (eql :axes (first item)))
    		   (setq axes (list (loop :for axis :in (rest item)
				       :collect (funcall process axis
							 (if (member :top-level
								     (getf (first (last preceding-properties))
									   :special))
							     '(:special (:top-level t))))))
    			 items rest-items))
	       (if (and axes (not items))
		   (error "Encountered axes with no function, operator or value to the left."))
	       (if (and (member (first items) '(⍺⍺ ⍵⍵))
			(not (member :operand-composition (getf (getf properties :special) :omit))))
		   (setq value-elements (cons (first items) value-elements)
			 value-props (cons '(:type (:array :operand-represented)) value-props)
			 items (rest items)))

	       (loop :while (not stopped)
		  :do (or (if (and (listp item) (eq :axes (first item)))
			      ;; if axes are encountered, process the axes and the preceding
			      ;; value as a new value
			      (multiple-value-bind (output properties remaining)
				  (funcall process items)
				(if (eq :array (first (getf properties :type)))
				    (setq items remaining
					  value-elements (cons output value-elements)
					  value-props (cons properties value-props)
					  stopped t))))
			  (if (and (listp item) (not (member (first item) '(:op :fn :axes))))
			      ;; if a closure is encountered, recurse to process it
			      (multiple-value-bind (output properties remaining)
				  (funcall process item (if (member :top-level
								    (getf (first (last preceding-properties))
									  :special))
							    '(:special (:top-level t))))
				(if (eq :array (first (getf properties :type)))
				    (setq items rest-items
					  value-elements (cons output value-elements)
					  value-props (cons properties value-props)))))
			  (multiple-value-bind (value-out value-properties)
			      (process-value item properties process idiom space)
			    (if value-out (setq items rest-items
						value-elements (cons value-out value-elements)
						value-props (cons value-properties value-props))))
			  (setq stopped t))))
	(if value-elements
	    (values (axes-enclose (output-value space (if (< 1 (length value-elements))
							  value-elements (first value-elements))
						value-props)
				  axes)
		    '(:type (:array :explicit))
		    items)
	    (values nil nil tokens))))))

(vex::composer-pattern-template
 (composer-pattern assign-axes assign-element assign-subprocessed)
 tokens space idiom process precedent properties preceding-properties special-props items item rest-items)

(composer-pattern composer-pattern-function (axes function-form function-props prior-items)
    ;; match a function like × or {⍵+10}, marking the beginning of a functional expression
    ((assign-axes axes)
     (setq prior-items items)
     (assign-element function-form function-props process-function (first (last preceding-properties)))
     (if (and (not function-form)
	      (listp (first items))
	      (eql :op (caar items))
	      (listp (first (last (first items)))))
	 (progn (setq items prior-items)
		(assign-element function-form function-props process-operator))))
  (let ((is-function (or (not (member :overloaded-operator (getf function-props :type)))
			 (let ((next (if items (multiple-value-list (funcall process items)))))
			   (not (member :function (getf (second next) :type)))))))
    (if (and function-form is-function)
	(values (if (or (not axes) (of-lexicon idiom :functions function-form))
		    ;; if axes are present, this is an n-argument function
		    (if (not (and (symbolp function-form) (is-workspace-function function-form)))
			function-form `(function (inws ,function-form)))
		    (let ((call-form (if (listp function-form)
					 function-form `(function ,(insym function-form)))))
		      `(apl-call :nafn ,call-form ,@(first axes))))
		(list :type (if (member :operator (getf function-props :type))
				(list :operator :inline-operator
				      (if (member :pivotal (getf function-props :type))
					  :pivotal :lateral))
				'(:function :inline-function))
		      :axes (or axes (getf function-props :axes)))
		items))))

(labels ((verify-lateral-operator-symbol (symbol space)
	   (if (symbolp symbol) (let ((symbol (intern (concatenate 'string "𝕆𝕃∇" (string symbol)))))
				  (if (fboundp (intern (string symbol) space)) symbol)))))
  (composer-pattern composer-pattern-lateral-composition
      (operator-axes operator-form operator-props operand-axes operand-form
		     operand-props symbol-referenced)
      ;; match a lateral function composition like +/, marking the beginning of a functional expression
      ((assign-axes operator-axes)
       (setq symbol-referenced (verify-lateral-operator-symbol item space))
       (assign-element operator-form operator-props process-operator '(:valence :lateral))
       (if operator-form (progn (assign-axes operand-axes)
				(assign-subprocessed operand-form operand-props
		 				     `(:special (:omit (:value-assignment :function-assignment
		 							:operation :operator-assignment
									:train-composition)
								       ,@include-lexvar-symbols))))))
    (if symbol-referenced
	;; call the operator constructor on the output of the operand constructor which integrates axes
	(values (list 'apl-compose :op (list 'inws symbol-referenced)
	  	      (if (listp operand-form)
	  		  operand-form
			  (if (characterp operand-form)
			      (let ((omega (gensym)) (alpha (gensym)))
	  			`(lambda (,omega &optional ,alpha)
	  			   (if ,alpha (apl-call :fn ,(resolve-function :dyadic operand-form)
	  		    				,omega ,alpha)
	  		    	       (apl-call :fn ,(resolve-function :monadic operand-form)
	  		    			 ,omega))))
			      operand-form)))
		'(:type (:function :operator-composed :lateral))
		items)
	(let ((operator (and (member :operator (getf operator-props :type))
	      		     (member :lateral (getf operator-props :type))
	      		     operator-form)))
	  (if operator
	      (if (eq :operator-self-reference operator-form)
		  (values `(apl-compose :op ∇oself ,operand-form)
			  '(:type (:function :operator-composed :lateral))
	      		  items)
		  (values (cons 'apl-compose (cons (intern (string-upcase operator)
							   *package-name-string*)
	      					   (funcall (funcall (resolve-operator :lateral operator)
	      							     operand-form (first operand-axes))
	      						    (first operator-axes))))
	      		  '(:type (:function :operator-composed :lateral))
	      		  items)))))))

(composer-pattern composer-pattern-unitary-operation (operator-axes operator-form operator-props)
    ;; match a unitary operator like $
    ((assign-axes operator-axes)
     (assign-element operator-form operator-props process-operator '(:valence :unitary)))
  (let ((operator (and (member :operator (getf operator-props :type))
		       (member :unitary (getf operator-props :type))
		       operator-form)))
    (if (resolve-operator :unitary operator)
	(values (funcall (resolve-operator :unitary operator) space (first operator-axes))
		'(:type (:array :evaluated))
		items))))

(defvar *composer-opening-patterns*)

(setq *composer-opening-patterns*
      '((:name :value :function composer-pattern-value)
	(:name :function :function composer-pattern-function)
	(:name :lateral-composition :function composer-pattern-lateral-composition)
	(:name :unitary-operator :function composer-pattern-unitary-operation)))

(composer-pattern value-assignment-by-function-result
    (asop asop-props fn-element fnel-specs function-axes symbol symbol-props symbol-axes)
    ;; "Match the assignment of a function result to a value, like a+←5."
    ((assign-element asop asop-props process-function '(:glyph ←))
     (if asop (assign-axes function-axes))
     (if asop (assign-subprocessed fn-element fnel-specs
				   `(:special (:omit (:value-assignment :function-assignment)
						     ,@include-lexvar-symbols))))
     (if fn-element (assign-axes symbol-axes))
     (if fn-element (assign-element symbol symbol-props process-value '(:symbol-overriding t))))
  (if (and fn-element symbol)
      (let ((fn-content (resolve-function :dyadic fn-element))
	    (fn-sym (or-functional-character fn-element :fn)))
	(values (if (not symbol-axes)
		    `(setq (inws ,symbol)
			   (apl-call ,fn-sym ,fn-content (inws ,symbol) ,precedent
				     ,@(if function-axes `((list ,@(first function-axes))))))
		    (enclose-axes `(inws, symbol) symbol-axes :set precedent
				  :set-by `(lambda (item item2) (apl-call ,fn-sym ,fn-content item item2))))
		'(:type (:array :assigned :by-result-assignment-operator))
		items))))

(composer-pattern value-assignment-by-selection
    (asop asop-props selection-form sform-specs)
    ;; "Match a selective value assignment like (3↑x)←5."
    ((assign-element asop asop-props process-function '(:glyph ←))
     (if (and asop (and (listp (first items))
			(not (member (caar items) '(:fn :op :axes)))))
	 (let ((items (first items)))
	   (assign-subprocessed selection-form sform-specs
				'(:special (:omit (:value-assignment :function-assignment))))))
     (if selection-form (setf items (rest items))))
  (if (and selection-form (listp selection-form) (eql 'apl-call (first selection-form)))
      (multiple-value-bind (sel-form sel-item placeholder set-form)
	  (generate-selection-form selection-form space)
	(if sel-form
	    ;; generate an array whose each cell is its row-major index, perform the subtractive function
	    ;; on it and then use assign-selected to assign new values to the cells at the remaining
	    ;; indices of the original array
	    (values (if sel-item (let ((item (gensym)) (indices (gensym)) (prec (gensym)))
				   `(let* ((,item ,sel-item)
					   (,placeholder (generate-index-array ,item))
					   (,prec ,precedent)
					   (,indices (enclose-atom ,sel-form))
					   ,@(if set-form `((,placeholder
							     (make-array nil :initial-element
									 (assign-selected (disclose ,item)
											  ,indices ,prec))))))
				      ,(funcall (lambda (form)
						  (if (not (or (symbolp sel-item)
							       (and (listp sel-item)
					      			    (eql 'inws (first sel-item))
					      			    (symbolp (second sel-item)))))
						      ;; the assigned value is returned at the end so
						      ;; things like a←⍳5 ⋄ b←(3⊃a)←30 ⋄ a b work
						      form `(progn (apl-assign ,sel-item ,form)
								   ,prec)))
						(or set-form `(assign-selected ,sel-item ,indices ,prec)))))
			(let ((output (gensym)))
			  `(let* ((,placeholder ,precedent)
				  (,output ,sel-form))
			     (if ,output (setf ,set-form ,output)))))
		    '(:type (:array :assigned)) items)))))

(composer-pattern value-assignment-standard
    (asop asop-props axes symbol symbol-props symbols symbols-props symbols-list preceding-type)
    ;; match a value assignment like a←1 2 3, part of an array expression
    ((setq preceding-type (getf (first preceding-properties) :type))
     (if (and (eq :array (first preceding-type))
	      (not (member :value-assignment (getf special-props :omit))))
	 (assign-element asop asop-props process-function '(:glyph ←)))
     (if asop (labels ((get-symbol-list (list &optional inner)
			 (let ((valid t))
			   (if (listp list)
			       ;; build list of symbols to be assigned values
			       ;; (multiple for stranded/nested assignment)
			       (let ((out-list (loop :while valid :for i
						  :in (if (and (not inner)
							       (not (eql 'avector (first list))))
							  list (rest list))
						  :collect (setq valid
								 (if (symbolp i)
								     (progn (if (not (member i symbols-list))
										(setq symbols-list
										      (cons i symbols-list)))
									    i)
								     (if (and (listp i) (eql 'inws (first i)))
									 (progn (setq symbols-list
										      (cons (second i)
											    symbols-list))
										i)
									 (get-symbol-list i t)))))))
				 (if valid out-list))))))
		(assign-axes axes)
		(let ((symbols-present t))
		  ;; collect each symbol to the left of ←, keeping them in (inws) forms if needed
      		  (loop :while symbols-present
      		     :do (multiple-value-bind (symbol-out symbol-props)
      			     (process-value item '(:symbol-overriding t) process idiom space)
			   (if (listp symbol-out)
			       (setq symbol-out (get-symbol-list symbol-out))
			       (if (symbolp symbol-out)
				   (progn (if (not (member symbol-out symbols-list))
					      (setq symbols-list (cons symbol-out symbols-list)))
					  (if (not (member symbol-out *idiom-native-symbols*))
					      (setq symbol-out (list 'inws symbol-out))))))
      			   (if (and symbol-out (or (symbolp symbol-out)
						   (and symbol-out (listp symbol-out))))
      			       (setq items rest-items
      				     symbols (cons symbol-out symbols)
      				     symbols-props (cons symbol-props symbols-props))
			       (setq symbols-present nil))))
		  (if (and (= 1 (length symbols))
			   (listp (first symbols)))
		      (setq symbols (first symbols)))
		  (setq symbol (if (symbolp (first symbols))
				   (if (eql 'inws (first symbols))
				       symbols (first symbols))
				   (if (listp (first symbols))
				       (caar symbols))))))))
  
  (if symbols
      ;; ensure symbol(s) are not bound to function values in the workspace, and
      ;; define them as dynamic variables if they're unbound there;
      ;; remove symbols from (inws) unless they're bare and thus idiom-native
      (values
       (progn (loop :for symbol :in symbols-list
		 :do (if (is-workspace-function symbol)
			 (fmakunbound (intern (string symbol) space)))
		   (if (and (not (boundp (intern (string symbol) space)))
			    (member :top-level (getf (first (last preceding-properties)) :special)))
		       (progn (proclaim (list 'special (intern (string symbol) space)))
			      (set (intern (string symbol) space) nil))))
	      (cond ((eql 'to-output symbol)
		     ;; a special case to handle ⎕← quad output
		     `(apl-output ,precedent :print-precision print-precision
				  :print-to output-stream :print-assignment t :with-newline t))
		    ((eql 'output-stream symbol)
		     ;; a special case to handle ⎕ost← setting the output stream; the provided string
		     ;; is interned in the current working package
		     (if (stringp precedent)
			 ;; setq is used instead of apl-assign because output-stream is a lexical variable
			 `(setq output-stream ,(intern precedent (package-name *package*)))
			 (if (listp precedent)
			     (destructuring-bind (vector-symbol package-string symbol-string) precedent
			       (if (and (eql 'avector vector-symbol)
					(stringp package-string)
					(stringp symbol-string))
				   ;; if the argument is a vector of two strings like ('APRIL' 'OUT-STR'),
				   ;; intern the symbol like (intern "OUT-STR" "APRIL")
				   `(setq output-stream ,(intern symbol-string package-string))
				   (error "Invalid assignment to ⎕OST.")))
			     (error "Invalid assignment to ⎕OST."))))
		    (t (if (symbolp symbol)
			   (set-workspace-alias space symbol nil))
		       (if axes (enclose-axes symbol axes :set precedent)
			   ;; enclose the symbol in (inws) so the (with-april-workspace) macro
			   ;; will correctly intern it, unless it's one of the system variables
			   `(apl-assign ,(if (not (and (listp symbols)
						       (not (eql 'inws (first symbols)))))
					     symbols (cons 'avector symbols))
					,precedent)))))
       '(:type (:array :assigned))
       items)))

(composer-pattern function-assignment (asop asop-props symbol symbol-props preceding-type)
    ;; "Match a function assignment like f←{⍵×2}, part of a functional expression."
    ((setq preceding-type (getf (first preceding-properties) :type))
     (if (eq :function (first preceding-type))
	 (assign-element asop asop-props process-function '(:glyph ←)))
     (if asop (assign-element symbol symbol-props process-value '(:symbol-overriding t))))
  (if asop (values (let* ((inverted (if (listp precedent) (invert-function precedent)))
			  (inverted-symbol (if inverted (intern (concatenate 'string "𝕚∇" (string symbol))))))
		     ;; dummy function initialization is carried out here as well as in the idiom's
		     ;; :lexer-postprocess method in order to catch assignments of composed functions like
		     ;; g←(3∘×); these are not recognized by :lexer-postprocess since it should not be aware
		     ;; of operator composition conventions in the code it receives
		     (if (is-workspace-value symbol)
		      	 (makunbound (intern (string symbol) space)))
		     (if (not (fboundp (intern (string symbol) space)))
			 (setf (symbol-function (intern (string symbol) space)) #'dummy-nargument-function))
		     (if inverted (progn (if (is-workspace-value inverted-symbol)
					     (makunbound (intern (string inverted-symbol) space)))
					 ;; TODO: should dummy function initialization for inverted
					 ;; functions still take place at this stage?
					 (setf (symbol-function (intern (string inverted-symbol) space))
					       #'dummy-nargument-function)))
		     (if (characterp precedent)
			 (if (or (resolve-function :monadic precedent)
				 (resolve-function :dyadic precedent))
			     (progn (set-workspace-alias space symbol precedent)
				    (format nil "~a aliases ~a" symbol precedent)))
			 (progn (set-workspace-alias space symbol nil)
				`(setf (symbol-function (quote (inws ,symbol))) ,precedent
				       ,@(if inverted `((symbol-function (quote (inws ,inverted-symbol)))
							,inverted))))))
		   '(:type (:function :assigned)) items)))

(composer-pattern operator-assignment (asop asop-props symbol symbol-props preceding-type)
    ;; "Match an operator assignment like f←{⍵×2}, part of a functional expression."
    ((setq preceding-type (getf (first preceding-properties) :type))
     (if (eq :operator (first preceding-type))
	 (assign-element asop asop-props process-function '(:glyph ←)))
     (if asop (assign-element symbol symbol-props process-value '(:symbol-overriding t))))
  (let ((operator-symbol (intern (concatenate 'string (if (member :pivotal preceding-type) "𝕆ℙ∇" "𝕆𝕃∇")
					      (string symbol)))))
    (if asop (progn (if (is-workspace-value symbol)
			(makunbound (intern (string symbol) space)))
		    (setf (symbol-function (intern (string operator-symbol) space))
			  #'dummy-nargument-function)
		    (values (if (characterp precedent)
				(if (or (resolve-operator :lateral precedent)
					(resolve-operator :pivotal precedent))
				    (progn (set-workspace-alias space symbol precedent)
					   (format nil "~a aliases ~a" symbol precedent)))
				(progn (set-workspace-alias space symbol nil)
				       `(setf (symbol-function (quote (inws ,operator-symbol)))
					      ,precedent)))
			    '(:type (:operator :assigned)) items)))))

(composer-pattern branch (asop asop-props branch-from from-props preceding-type)
    ;; "Match a branch-to statement like →1 or a branch point statement like 1→⎕."
    ((setq preceding-type (getf (first preceding-properties) :type))
     (if (eq :array (first preceding-type))
	 (assign-element asop asop-props process-function '(:glyph →)))
     (if asop (assign-element branch-from from-props process-value)))
  (if asop (progn
	     (if (listp precedent)
		 (if (loop :for item :in precedent :always (and (listp item) (eql 'inws (first item))))
		     (setq precedent (mapcar #'second precedent))
		     (if (eql 'inws (first precedent))
			 (setq precedent (second precedent)))))
	     (values
	      (if (and branch-from (eql 'to-output precedent))
		  ;; if this is a branch point statement like X→⎕, do the following:
		  (if (integerp branch-from)
		      ;; if the branch is designated by an integer like 5→⎕
		      (let ((branch-symbol (gensym "AB"))) ;; AB for APL Branch
			(setf *branches* (cons (list branch-from branch-symbol) *branches*))
			branch-symbol)
		      ;; if the branch is designated by a symbol like doSomething→⎕
		      (if (symbolp branch-from)
			  (progn (setf *branches* (cons branch-from *branches*))
				 branch-from)
			  (error "Invalid left argument to →; must be a single integer value or a symbol.")))
		  ;; otherwise, this is a branch-to statement like →5 or →doSomething
		  (if (or (integerp precedent)
			  (symbolp precedent))
		      ;; if the target is an explicit symbol as in →mySymbol, or explicit index
		      ;; as in →3, just pass the symbol through
		      (list 'go precedent)
		      (if (loop :for item :in (rest precedent)
			     :always (or (symbolp item)
					 (and (listp item) (eql 'inws (first item)))))
			  ;; if the target is one of an array of possible destination symbols...
			  (if (integerp branch-from)
			      ;; if there is an explicit index to the left of the arrow,
			      ;; grab the corresponding symbol unless the index is outside the
			      ;; array's scope, in which case a (list) is returned so nothing is done
			      (if (< 0 branch-from (length (rest precedent)))
				  (list 'go (second (nth (1- branch-from) (rest precedent))))
				  (list 'list))
			      ;; otherwise, there must be an expression to the left of the arrow, as with
			      ;; (3-2)→tagOne tagTwo, so pass it through for the postprocessor
			      (list 'go (mapcar #'second (rest precedent))
				    branch-from))
			  (list 'go precedent))))
	      '(:type (:branch)) items))))

(composer-pattern train-composition (center center-props is-center-function left left-props preceding-type)
    ;; "Match a train function composition like (-,÷)."
    ((setq preceding-type (getf (first preceding-properties) :type))
     (if (eq :function (first preceding-type))
	 (progn (assign-subprocessed center center-props
				     `(:special (:omit (:value-assignment :function-assignment
							:train-composition)
						 ,@include-lexvar-symbols)))
		(setq is-center-function (eq :function (first (getf center-props :type))))
		(if is-center-function
		    (assign-subprocessed left left-props
					 `(:special (:omit (:value-assignment
							    :function-assignment :branch :operator-assignment
							    :value-assignment-by-selection :operation)
							   ,@include-lexvar-symbols)))))))
  (if is-center-function
      (if (not left)
	  ;; if there's no left function, match an atop composition like 'mississippi'(⍸∊)'sp'
	  (let* ((omega (gensym)) (alpha (gensym)) (right precedent)
		 (left-fn-monadic (if (listp center)
				      center (resolve-function
					      :monadic (if (not (symbolp center))
							   center (intern (string center) space)))))
		 (right-fn-monadic (if (and (listp right) (eql 'function (first right)))
				       right (resolve-function :monadic right)))
		 (right-fn-dyadic (if (and (listp right) (eql 'function (first right)))
 				      right (resolve-function :dyadic right))))
	    (values `(lambda (,omega &optional ,alpha)
		       (if ,alpha (apl-call ,(or-functional-character left :fn)
					    ,left-fn-monadic (apl-call ,(or-functional-character right :fn)
								       ,right-fn-dyadic ,omega ,alpha))
			   (apl-call ,(or-functional-character left :fn)
				     ,left-fn-monadic (apl-call ,(or-functional-character right :fn)
								,right-fn-monadic ,omega))))
		    (list :type (list :function :train-atop-composition))))
	  ;; if there's a left function, match a fork composition like (-,÷)5
	  (destructuring-bind (right omega alpha center)
	      (list precedent (gensym) (gensym)
 		    (if (listp center)
 			center (resolve-function :dyadic (if (not (symbolp center))
 							     center (intern (string center) space)))))
	    ;; train composition is only valid when there is only one function in the precedent
	    ;; or when continuing a train composition as for (×,-,÷)5; remember that operator-composed
	    ;; functions are also valid as preceding functions, as with (1+-∘÷)
	    (if (and center (or (= 1 (length preceding-properties))
				(and (member :function (getf (first preceding-properties) :type))
				     (member :operator-composed (getf (first preceding-properties) :type)))
 				(member :train-fork-composition (getf (first preceding-properties) :type))))
 		;; functions are resolved here, failure to resolve indicates a value in the train
 		(let ((right-fn-monadic (if (and (listp right) (eql 'function (first right)))
 					    right (resolve-function :monadic right)))
 		      (right-fn-dyadic (if (and (listp right) (eql 'function (first right)))
 					   right (resolve-function :dyadic right)))
 		      (left-fn-monadic (if (and (listp left) (eql 'function (first left)))
 					   left (resolve-function :monadic left)))
 		      (left-fn-dyadic (if (and (listp left) (eql 'function (first left)))
 					  left (resolve-function :dyadic left))))
 		  (values `(lambda (,omega &optional ,alpha)
 			     (if ,alpha (apl-call ,(or-functional-character center :fn) ,center
 						  (apl-call ,(or-functional-character right :fn)
 							    ,right-fn-dyadic ,omega ,alpha)
 						  ,(if (not left-fn-dyadic)
 						       left `(apl-call ,(or-functional-character left :fn)
 								       ,left-fn-dyadic ,omega ,alpha)))
 				 (apl-call ,(or-functional-character center :fn) ,center
 					   (apl-call ,(or-functional-character right :fn)
						     ,right-fn-monadic ,omega)
 					   ,(if (not left-fn-monadic)
 						left `(apl-call ,(or-functional-character left :fn)
 								,left-fn-monadic ,omega)))))
			  (list :type (list :function :train-fork-composition))
			  items)))))))

(composer-pattern lateral-inline-composition
    (operator operator-props left-operand-axes left-operand left-operand-props left-value
	      left-value-props prior-items right-operand-axes preceding-type)
    ;; Match an inline lateral operator composition like +{⍺⍺ ⍵}5.
    ((setq preceding-type (getf (first preceding-properties) :type))
     (assign-element operator operator-props process-operator '(:valence :lateral))
     (if operator (progn (assign-axes left-operand-axes)
			 (setq prior-items items)
			 (assign-element left-operand left-operand-props process-function)
			 ;; if the next function is symbolic, assign it uncomposed;
			 ;; this is needed for things like ∊∘.+⍨10 2 to work correctly
			 (if (and items (not (member :symbolic-function (getf left-operand-props :type))))
			     (progn (setq items prior-items item (first items) rest-items (rest items))
				    ;; the special :omit property makes it so that the pattern matching
				    ;; the operand may not be processed as a value assignment, function
				    ;; assignment or operation, which allows for expressions like
				    ;; fn←5∘- where an operator-composed function is assigned
				    (assign-subprocessed
				     left-operand left-operand-props
				     `(:special (:omit (:value-assignment :function-assignment :operation)
						       ,@include-lexvar-symbols)))
				    ;; try getting a value on the left, as for 3 +{⍺ ⍺⍺ ⍵} 4
				    (assign-subprocessed
				     left-value left-value-props
				     '(:special (:omit (:value-assignment :function-assignment
							:operation)))))))))
  (if operator
      ;; get left axes from the left operand and right axes from the precedent's properties so the
      ;; functions can be properly curried if they have axes specified
      (let ((left-operand (insym left-operand))
	    ;; (left-operand-axes (first (getf (second properties) :axes)))
	    (omega (gensym)) (alpha (gensym)))
	;; single character values are passed within a (:char) form so they aren't interpreted as
	;; functional glyphs by the (resolve-function) calls
	(if (and (characterp left-operand) (member :array (getf left-operand-props :type)))
	    (setq left-operand (list :char left-operand)))
	(values (if (and (listp operator) (member :lateral (getf operator-props :type)))
		    `(apl-call :fn (apl-compose :op ,operator
						,(if (listp left-operand)
						     left-operand
						     (if (characterp left-operand)
							 `(lambda (,omega &optional ,alpha)
							    (if ,alpha
								(apl-call :fn ,(resolve-function
										:dyadic left-operand)
									  ,omega ,alpha)
								(apl-call :fn ,(resolve-function
										:monadic left-operand)
									  ,omega)))
							 left-operand)))
			       ,precedent ,@(if left-value (list left-value))))
		'(:type (:function :operator-composed :lateral :inline)) items))))

(composer-pattern pivotal-composition
    (operator operator-props left-operand-axes left-operand left-operand-props left-value
	      left-value-props prior-items right-operand-axes preceding-type)
    ;; Match a pivotal function composition like ×.+, part of a functional expression.
    ;; It may come after either a function or an array, since some operators take array operands.
    ((setq preceding-type (getf (first preceding-properties) :type))
     (assign-element operator operator-props process-operator '(:valence :pivotal))
     (if operator (progn (assign-axes left-operand-axes)
			 (setq prior-items items)
			 (assign-element left-operand left-operand-props process-function)
			 ;; if the next function is symbolic, assign it uncomposed;
			 ;; this is needed for things like ∊∘.+⍨10 2 to work correctly
			 (if (not (member :symbolic-function (getf left-operand-props :type)))
			     (progn (setq items prior-items item (first items) rest-items (rest items))
				    ;; the special :omit property makes it so that the pattern matching
				    ;; the operand may not be processed as a value assignment, function
				    ;; assignment or operation, which allows for expressions like
				    ;; fn←5∘- where an operator-composed function is assigned
				    (assign-subprocessed
				     left-operand left-operand-props
				     '(:special (:omit (:value-assignment :function-assignment
							:operation :train-composition)))))))))
  (if operator
      ;; get left axes from the left operand and right axes from the precedent's properties so the
      ;; functions can be properly curried if they have axes specified
      (let ((right-operand (insym precedent))
	    (right-operand-axes (getf (first preceding-properties) :axes))
	    (left-operand (insym left-operand))
	    (omega (gensym)) (alpha (gensym)))
	;; single character values are passed within a (:char) form so they aren't interpreted as
	;; functional glyphs by the (resolve-function) calls
	(if (and (characterp left-operand) (member :array (getf left-operand-props :type)))
	    (setq left-operand (list :char left-operand)))
	(values (if (or (symbolp operator) (and (listp operator)
						(member :pivotal (getf operator-props :type))))
		    `(apl-compose :op ,(if (eq :operator-self-reference operator)
					   '∇oself (if (listp operator)
						       operator (list 'inws operator)))
				  ,(if (listp left-operand)
				       left-operand
				       (if (characterp left-operand)
					   `(lambda (,omega &optional ,alpha)
					      (if ,alpha
						  (apl-call :fn ,(resolve-function :dyadic left-operand)
							    ,omega ,alpha)
						  (apl-call :fn ,(resolve-function :monadic left-operand)
							    ,omega)))))
				  ,(if (listp right-operand)
				       left-operand
				       (if (characterp right-operand)
					   `(lambda (,omega &optional ,alpha)
					      (if ,alpha
						  (apl-call :fn ,(resolve-function :dyadic right-operand)
							    ,omega ,alpha)
						  (apl-call :fn ,(resolve-function :monadic right-operand)
							    ,omega))))))
		    (cons 'apl-compose (cons (intern (string-upcase operator) *package-name-string*)
					     (funcall (funcall (resolve-operator :pivotal operator)
							       ;; TODO: taking (first) of axes
							       ;; eliminates possible future functions
							       ;; that take more than one axis argument
							       left-operand (first left-operand-axes)
							       right-operand (first right-operand-axes))
						      right-operand left-operand))))
		'(:type (:function :operator-composed :pivotal)) items))))

(composer-pattern operation
    ;; "Match an operation on values like 1+1 2 3, ⍳9 or +/⍳5, these operations are the basis of APL."
    (function-axes fn-element function-props is-function value value-props prior-items preceding-type)
    ((setq preceding-type (getf (first preceding-properties) :type))
     (if (eq :array (first preceding-type))
	 (progn (assign-subprocessed fn-element function-props
				     `(:special (:omit (:function-assignment :value-assignment-by-selection
									     :operand-composition
									     :train-composition :operation)
						       ,@include-lexvar-symbols)))
		(setq is-function (eq :function (first (getf function-props :type)))
		      prior-items items)
		(if is-function (assign-subprocessed value value-props
						     `(:special (:omit (:value-assignment :function-assignment
									:value-assignment-by-selection :branch
									:value-assignment-by-function-result
									:lateral-composition
									:lateral-inline-composition
		 							:operation :operator-assignment))
						       ,@include-lexvar-symbols
						       :valence :lateral)))
		(if (not (eq :array (first (getf value-props :type))))
		    (setq items prior-items value nil))
		(if (and (not function-axes) (member :axes function-props))
		    (setq function-axes (getf function-props :axes))))))
  (if is-function (let* ((fn-content (if (or (functionp fn-element)
					     (member fn-element '(⍺⍺ ⍵⍵ ∇ ∇∇))
					     (and (listp fn-element)
						  (or (eql 'function (first fn-element))
						      (eql 'ifn (first fn-element)))))
					 fn-element (or (resolve-function (if value :dyadic :monadic)
									  (insym fn-element))
							(resolve-function :symbolic fn-element))))
			 ;; the ∇ symbol resolving to :self-reference generates the #'∇self function used
			 ;; as a self-reference by lambdas invoked through the (alambda) macro
			 (fn-content (if (not (eql '∇ fn-content))
					 fn-content '#'∇self))
			 (fn-sym (or-functional-character fn-element :fn)))
		    (values `(apl-call ,fn-sym ,fn-content ,precedent ,@(if value (list value))
				       ,@(if function-axes `((list ,@(first function-axes)))))
			    '(:type (:array :evaluated)) items))))

(defvar *composer-following-patterns*)

(setq *composer-following-patterns*
      '((:name :value-assignment-by-function-result :function value-assignment-by-function-result)
	(:name :value-assignment-by-selection :function value-assignment-by-selection)
	(:name :value-assignment-standard :function value-assignment-standard)
	(:name :function-assignment :function function-assignment)
	(:name :operator-assignment :function operator-assignment)
	(:name :branch :function branch)
	(:name :train-composition :function train-composition)
	(:name :lateral-inline-composition :function lateral-inline-composition)
	(:name :pivotal-composition :function pivotal-composition)
	(:name :operation :function operation)))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Aplesque -*-
;;;; aplesque.lisp

(in-package #:aplesque)

"A set of functions implementing APL-like array operations. Used to provide the functional backbone of the April language."

(defun get-free-threads ()
  (let ((workers (lparallel.kernel::workers lparallel:*kernel*)))
    (loop :for w :across workers :when (not (lparallel.kernel::running-category w))
       :counting w :into total :finally (return total))))

(defmacro xdotimes (object clause &body body)
  (let ((asym (gensym)) (eltype (gensym)) (free-threads (gensym)))
    `(let* ((,asym ,object)
	    (,eltype (element-type ,asym))
	    (,free-threads (get-free-threads)))
       (if (or (not lparallel:*kernel*)
	       (= 0 ,free-threads)
	       (eql 'bit ,eltype)
	       (and (listp ,eltype)
	   	    (member 'unsigned-byte ,eltype)
	   	    (> 7 (second ,eltype))))
	   (dotimes ,clause ,@body)
	   (pdotimes ,(append clause (list nil free-threads)) ,@body)))))

(defmacro ydotimes (object clause &body body)
  (let ((asym (gensym)) (eltype (gensym)) (free-threads (gensym))
	(iterations (gensym)) (dividend (gensym)) (remainder (gensym))
	(x (gensym)) (y (gensym)))
    `(let* ((,asym ,object)
	    (,eltype (element-type ,asym))
	    (,free-threads (get-free-threads)))
       (if (or (not lparallel:*kernel*)
	       (= 0 ,free-threads))
	   (dotimes ,clause ,@body)
	   (if (or (eql 'bit ,eltype)
		   (and (listp ,eltype)
	   		(member 'unsigned-byte ,eltype)
	   		(> 7 (second ,eltype))))
	       (let ((,iterations (if (eq 'bit ,eltype)
				      8 (/ 8 (second ,eltype)))))
		 ;; (print (list :iter ,iterations))
		 (multiple-value-bind (,dividend ,remainder) (ceiling ,(second clause) ,iterations)
		   ;; (print (list :dd ,dividend ,remainder))
		   (pdotimes ,(list x dividend nil free-threads)
		     (dotimes (,y (+ ,iterations (if (< ,x (1- ,dividend))
						     0 ,remainder)))
		       (let ((,(first clause) (+ ,y (* ,x ,iterations))))
			 ,@body)))))
	       (pdotimes ,(append clause (list nil free-threads)) ,@body))))))

(defun get-dimensional-factors (dimensions)
  (let ((factor) (last-index))
    (reverse (loop :for d :in (reverse dimensions) :for dx :from 0
		:collect (setq factor (if (= 0 dx) 1 (* factor last-index)))
		:do (setq last-index d)))))

(defun rmi-from-subscript-vector (array subscripts)
  "Derive an array's row-major index from a vector of subscripts."
  (if (not (vectorp subscripts))
      subscripts
      (let ((rank (rank array))
	    (dims (reverse (dims array)))
	    (length (length subscripts)))
	(cond ((/= length rank) (error "Wrong number of subscripts, ~W, for array of rank ~W." length rank))
	      ((= 1 rank) (aref subscripts 0))
	      (t (let ((result 0) (factor 1) (simple-vector t))
		   (loop :for i :from (1- length) :downto 0 :while simple-vector
		      :do (if (not (integerp (aref subscripts i)))
			      (setq simple-vector nil)
			      (if (<= (first dims) (aref subscripts i))
				  (error "Invalid index for dimension ~W." (1+ i))
				  (setq result (+ result (* factor (aref subscripts i)))
					factor (* factor (first dims))
					dims (rest dims)))))
		   (if simple-vector result)))))))

(defun ranges-to-rmi-vector (ranges factors &optional vector count)
  (let* ((count (or count 0))
	 (range (first ranges))
	 (factor (first factors))
	 (vlength (reduce #'* (loop :for r :in ranges :collect (1+ (- (second r) (first r))))))
	 (vector (or vector (make-array vlength :fill-pointer 0))))
    (loop :for i :from (first range) :to (second range)
       :do (if (not (rest ranges))
	       (vector-push (+ count (* i factor)) vector)
	       (ranges-to-rmi-vector (rest ranges) (rest factors) vector (+ count (* i factor)))))
    vector))

(defun segment-area (size section-count)
  (let* ((section-count (min section-count size))
	 (division-size (/ size section-count))
	 (start-points (make-array (list section-count)))
	 (section-lengths (make-array (list section-count))))
    (loop :for i :below section-count :do (setf (aref start-points i) (floor (* i division-size))))
    (loop :for i :below section-count :do (setf (aref section-lengths i)
						(- (if (= i (1- section-count))
						       size (aref start-points (1+ i)))
						   (aref start-points i))))
    (values start-points section-lengths section-count)))

(defun varef (array subscripts)
  "Reference an element in an array according to a vector of subscripts."
  (row-major-aref array (rmi-from-subscript-vector array subscripts)))

(defun (setf varef) (new-value array subscripts)
  "Set an element in an array according to a vector of subscripts."
  (setf (row-major-aref array (rmi-from-subscript-vector array subscripts))
	new-value))

(defun is-unitary (value)
  "Check whether this array has only one member, returning true if the argument is not an array."
  (or (not (arrayp value))
      (= 1 (size value))))

(defun enclose (item)
  (if (not (arrayp item))
      item (make-array nil :initial-element item)))

(defun enclose-atom (item)
  "Enclose non-array values, passing through arguments that are already arrays."
  (if (arrayp item)
      item (make-array 1 :initial-element item :element-type (assign-element-type item))))

(defun nest (input)
  "Enclose simple arrays and return nested arrays as-is."
  (if (or (not (arrayp input))
	  (and (eq t (array-element-type input))
	       (not (loop :for item :across (make-array (size input) :element-type t :displaced-to input)
		       :never (arrayp item)))))
      input (make-array nil :initial-contents input)))

(defun disclose (array)
  "Disclose a scalar nested array."
  (if (or (< 0 (rank array))
	  (not (arrayp array)))
      array (aref array)))

(defun disclose-unitary (item)
  "If the argument is an array with only one member, disclose it, otherwise do nothing."
  (if (not (and (arrayp item) (is-unitary item)))
      item (row-major-aref item 0)))

(defun get-first-or-disclose (omega)
  (if (not (arrayp omega))
      omega (if (= 0 (rank omega))
		(aref omega) (if (< 0 (size omega))
				 (row-major-aref omega 0)
				 (apl-array-prototype omega)))))

(defun scale-array (unitary to-match &optional axis)
  "Scale up a 1-element array to fill the dimensions of the given array."
  (let ((match-dims (dims to-match)))
    (make-array (if axis (loop :for this-dim :in match-dims :for tdix :from 0
			    :collect (if (= tdix axis) 1 this-dim))
		    match-dims)
		:element-type (element-type unitary) :initial-element (aref unitary 0))))

(defun array-promote (input)
  "Promote an array to the next rank. The existing content will occupy 1 unit of the new dimension."
  (let ((output (make-array (cons 1 (dims input))
			    :element-type (element-type input))))
    (across input (lambda (elem coords)
		    (setf (apply #'aref output 0 coords)
			  elem)))
    output))

(defun copy-nested-array (array)
  "Copy an array along with nested sub-arrays within it."
  (if (not (arrayp array))
      array (if (not (eq t (element-type array)))
		(copy-array array)
		(let ((output (make-array (dims array))))
		  (dotimes (i (size output))
		    (setf (row-major-aref output i)
			  (copy-nested-array (row-major-aref array i))))
		  output))))

(defun array-to-list (input)
  "Convert array to list."
  (if (or (not (arrayp input))
	  (= 0 (rank input)))
      (list (disclose input))
      (let* ((dimensions (dims input))
	     (depth (1- (length dimensions)))
	     (indices (make-list (1+ depth) :initial-element 0)))
	(labels ((recurse (n)
		   (loop :for j :below (nth n dimensions)
		      :do (setf (nth n indices) j)
		      :collect (if (= n depth)
				   ;; (let ((item (apply #'aref input indices)))
				   ;;   (if (arrayp item)
				   ;; 	 (array-to-list item)
				   ;; 	 item))
				   (apply #'aref input indices)
				   (recurse (1+ n))))))
	  (recurse 0)))))

(defun apl-array-prototype (array)
  "Returns the default element for an array based on that array's first element (its prototype in array programming terms); blank spaces in the case of a character prototype and zeroes for others."
  (labels ((derive-element (input)
	     (if (characterp input)
		 #\  (if (not (arrayp input))
			 0 (if (= 0 (size input))
			       (make-array (dims input))
			       (derive-element (row-major-aref input 0)))))))
    (if (not (arrayp array))
	(derive-element array)
	(if (= 0 (size array))
	    (if (eql 'character (element-type array))
		#\  0)
	    (let ((first-element (row-major-aref array 0)))
	      (if (not (arrayp first-element))
		  (derive-element first-element)
		  (funcall (if (< 0 (rank first-element))
			       #'identity (lambda (item) (make-array nil :initial-element item)))
			   (let ((first-element (if (< 0 (rank first-element))
						    first-element (aref first-element))))
			     (if (and (arrayp first-element)
				      (= 0 (size first-element)))
				 first-element
				 (make-array (dims first-element)
					     :element-type (element-type first-element)
					     :initial-element (derive-element first-element)))))))))))

(defun assign-element-type (item)
  "Find a type suitable for an APL array to hold a given item."
  (typecase item
    (bit 'bit)
    (character 'character)
    (integer (list 'integer (min 0 item)
		   (max 0 item)))
    (single-float 'single-float)
    (double-float 'double-float)
    (t t)))

(defun type-in-common (&rest types)
  "Find a type for an array that may hold elements from arrays of a set of given types; effectively the most efficient compatible type among the array types."
  (let ((type))
    (loop :for a :in types
       :do (let ((this-type a))
	     (setq type (if type (if (listp type)
				     (cond ((eql 'bit this-type)
					    (if (and (eql 'integer (first type))
						     (= 0 (third type)))
						(list 'integer (second type) 1)
						type))
					   ((eql 'fixnum this-type)
					    'fixnum)
					   ((eql 'bignum this-type)
					    'bignum)
					   ((not (listp this-type))
					    t)
					   ((eql 'integer (first type))
					    (cond ((eql 'integer (first this-type))
						   (list 'integer (min 0 (second this-type)
								       (second type))
							 (max 0 (third this-type)
							      (third type))))
						  (t t)))
					   ((eql 'unsigned-byte (first type))
					    (cond ((eql 'unsigned-byte (first this-type))
						   (list 'unsigned-byte (max (second type)
									     (second this-type))))
						  ((eql 'signed-byte (first this-type))
						   'fixnum)
						  (t t)))
					   ((eql 'signed-byte (first type))
					    (cond ((eql 'signed-byte (first this-type))
						   (list 'signed-byte (max (second type)
									   (second this-type))))
						  ((eql 'unsigned-byte (first this-type))
						   'fixnum)
						  (t t)))
					   (t t))
				     (cond ((eql type this-type)
					    type)
					   ((eql 'bit type)
					    (cond ((eql 'fixnum this-type)
						   'fixnum)
						  ((eql 'bignum this-type)
						   'bignum)
						  ((listp this-type)
						   (if (eql 'integer (first this-type))
						       (list (first this-type)
							     (second this-type)
							     (max 1 (third this-type)))
						       this-type))
						  (t t)))
					   ((eql 'fixnum type)
					    (cond ((or (eql 'bit this-type)
						       (listp this-type))
						   'fixnum)
						  ((eql 'bignum this-type)
						   'bignum)
						  (t t)))
					   ((eql 'bignum type)
					    (cond ((or (eql 'bit this-type)
						       (eql 'fixnum this-type)
						       (listp this-type))
						   'bignum)
						  (t t)))
					   ((eql 'single-float type)
					    (cond ((eql 'double-float this-type)
						   'double-float)
						  (t t)))
					   ((eql 'double-float type)
					    (cond ((eql 'single-float this-type)
						   'double-float)
						  (t t)))
					   (t t)))
			    this-type))))
    type))

(defun across (input function &key elements indices reverse-axes count ranges
				foreach finally (depth 0) (dimensions (dims input)))
  "Iterate across a range of elements in an array, with the option of specifying which elements within each dimension to process."
  (let* ((proceeding t)
	 (indices (or indices (loop :for i :below (rank input) :collect 0)))
	 (first-of-elements (first elements))
	 (elems (if (listp first-of-elements)
		    first-of-elements (list first-of-elements)))
	 (range (first ranges)))
    (flet ((process-this (elix)
	     (setf (nth depth indices) elix)
	     (if (< depth (1- (rank input)))
		 ;; if the halt-if-true value is output by the function, traversal across the array
		 ;; will end by means of nullifying the proceeding variable; this will result in
		 ;; a nil return value from the across function which will stop its recursive parents
		 (multiple-value-bind (still-proceeding new-count)
		     (across input function :dimensions dimensions :elements (rest elements)
			     :indices indices :reverse-axes reverse-axes :depth (1+ depth)
			     :count count :ranges (rest ranges) :foreach foreach)
		   (setq proceeding still-proceeding count new-count))
		 (multiple-value-bind (output halt-if-true)
		     (funcall function (apply #'aref input indices)
			      indices)
		   (declare (ignore output))
		   (if (and foreach (functionp foreach)) (funcall foreach))
		   (if count (decf count))
		   (if (or halt-if-true (and count (> 1 count)))
		       (setq proceeding nil))))))
      (if (= 0 (rank input))
	  (funcall function (disclose input) nil)
	  (if elems (if (listp (rest elems))
			(loop :for el :in elems :while proceeding :do (process-this el))
			(if (< (first elems) (rest elems))
			    (loop :for el :from (first elems)
			       :to (rest elems) :do (process-this el))
			    (loop :for el :from (first elems) :downto (rest elems)
			       :do (process-this el))))
	      (if (and range (or (and (not (second range))
				      (= (nth depth dimensions) (first range)))
				 (eq t (reduce #'> range))))
		  (loop :for el :from (min (first range) (1- (nth depth dimensions)))
		     :downto (or (second range) 0)
		     :while proceeding :do (process-this el))
		  (loop :for el :from (or (first range) 0)
		     :to (or (second range) (1- (nth depth dimensions)))
		     :while proceeding :do (process-this el)))))
      (if (and finally (functionp finally)) (funcall finally))
      (values proceeding count))))

(defun apply-scalar (function omega &optional alpha axes is-boolean)
  "Apply a scalar function over an array or arrays as appropriate for the shape of the argument(s)."
  (let* ((orank (rank omega)) (arank (rank alpha))
	 (axes (if axes (enclose-atom axes)))
	 (oscalar (if (is-unitary omega) (get-first-or-disclose omega)))
	 (ascalar (if (is-unitary alpha) (get-first-or-disclose alpha)))
	 (output-dims (dims (if axes (if (> arank orank) alpha omega)
				(if oscalar alpha omega))))
	 (output-type (if (or (not is-boolean)
			      (not (= orank arank))
			      (not (and oscalar ascalar)))
			  t 'bit))
	 ;; for boolean arrays, check whether the output will directly hold the array contents
	 (output (if (not (and oscalar (or ascalar (not alpha))))
		     (make-array output-dims :element-type output-type))))
    (flet ((promote-or-not (item)
	     ;; function for wrapping output in a vector or 0-rank array if the input was thusly formatted
	     (declare (dynamic-extent item))
	     (if (not (or (arrayp omega) (arrayp alpha)))
		 item (let ((output (make-array (loop :for i :below (max orank arank) :collect 1))))
			(setf (row-major-aref output  0) item)
			output))))
      (if (not alpha)
	  ;; if the function is being applied monadically, map it over the array
	  ;; or recurse if an array is found inside
	  (if oscalar (setq output (promote-or-not (if (arrayp oscalar)
						       (apply-scalar function oscalar alpha axes is-boolean)
						       (funcall function oscalar))))
	      (xdotimes output (i (size omega))
		 (setf (row-major-aref output i) (nest (apply-scalar function (row-major-aref omega i))))))
	  (if (and oscalar ascalar)
	      ;; if both arguments are scalar or 1-element, return the output of the function on both,
	      ;; remembering to promote the output to the highest rank of the input, either 0 or 1 if not scalar
	      (setq output (promote-or-not (if (or (arrayp oscalar) (arrayp ascalar))
					       (apply-scalar function oscalar ascalar axes is-boolean)
					       (funcall function oscalar ascalar))))
	      (if (or oscalar ascalar
		      (and (= orank arank)
			   (loop :for da :in (dims alpha) :for do :in (dims omega) :always (= da do))))
		  ;; map the function over identically-shaped arrays
		  (xdotimes output (i (size (if oscalar alpha omega)))
		    (setf (row-major-aref output i)
			  (apply-scalar function (or oscalar (disclose (row-major-aref omega i)))
					(or ascalar (disclose (row-major-aref alpha i))))))
		  ;; if axes are given, go across the higher-ranked function and call the function on its
		  ;; elements along with the appropriate elements of the lower-ranked function
		  (if axes (destructuring-bind (lowrank highrank &optional omega-lower)
			       (if (> orank arank) (list alpha omega) (list omega alpha t))
			     (if (loop :for a :across axes :for ax :from 0
 				    :always (and (< a (rank highrank))
						 (= (nth a (dims highrank)) (nth ax (dims lowrank)))))
				 (let ((lrc (loop :for i :below (rank lowrank) :collect 0)))
				   (across highrank (lambda (elem coords)
						      (loop :for a :across axes :for ax :from 0
							 :do (setf (nth ax lrc) (nth a coords)))
						      (setf (apply #'aref output coords)
							    (nest (if omega-lower
								      (funcall function elem
									       (apply #'aref lowrank lrc))
								      (funcall function
									       (apply #'aref lowrank lrc)
									       elem)))))))
				 (error "Incompatible dimensions or axes.")))
		      (error "Mismatched array sizes for scalar operation.")))))
      output)))

(defun array-compare (item1 item2)
  "Perform a deep comparison of two APL arrays, which may be multidimensional or nested."
  (if (not (arrayp item1))
      (if (not (arrayp item2))
	  (or (and (numberp item1)
		   (numberp item2)
		   (= item1 item2))
	      (and (characterp item1)
		   (characterp item2)
		   (char= item1 item2))))
      (if (and (arrayp item2) (= (rank item1) (rank item2))
	       (let ((dims1 (dims item1))
		     (dims2 (dims item2)))
		 (if (loop :for d :below (length dims1)
			:always (= (nth d dims1) (nth d dims2)))
		     (loop :for i :below (size item1)
			:always (array-compare (row-major-aref item1 i) (row-major-aref item2 i))))))
	  (let ((match t))
	    (dotimes (i (size item1))
	      (let ((item (row-major-aref item1 i))
		    (alternate (row-major-aref item2 i)))
		(setq match (and match (or (and (arrayp item) (arrayp alternate)
						(array-compare item alternate))
					   (and (numberp item) (numberp alternate)
						(= item alternate))
					   (and (characterp item) (characterp alternate)
						(char= item alternate)))))))
	    match))))

(defun array-depth (input &optional layer (uniform t) possible-depth)
  "Find the maximum depth of nested arrays within an array."
  (let* ((first-level (not layer))
	 (layer (if layer layer 1))
	 (new-layer layer))
    (dotimes (i (size input))
      (let ((item (row-major-aref input i)))
	(if (arrayp item)
	    (multiple-value-bind (next-layer new-uniform new-possible-depth)
		(array-depth item (1+ layer) uniform possible-depth)
	      (setq new-layer (max new-layer next-layer)
		    uniform new-uniform
		    possible-depth new-possible-depth))
	    (if (not possible-depth) (setq possible-depth new-layer)
		(if (/= layer possible-depth) (setq uniform nil))))))
    (values (funcall (if (and first-level (not uniform)) #'- #'identity)
		     new-layer)
	    uniform possible-depth)))

(defun section (input dimensions &key (inverse nil) (populator nil))
  "Take a subsection of an array of the same rank and given dimensions as per APL's ↑ function, or invert the function as per APL's ↓ function to take the elements of an array excepting a specific dimensional range."
  ;; (print (list :iin input dimensions))
  (if (= 0 (rank input))
      (if inverse (make-array 0)
	  (let* ((prototype (apl-array-prototype input))
		 (output (make-array (array-to-list dimensions)
				     :element-type (assign-element-type input)
				     :initial-element (if (not (arrayp prototype)) prototype))))
	    (if (< 0 (size output))
		(setf (row-major-aref output 0) input))
	    (if (arrayp prototype)
		(loop :for i :from 1 :to (1- (size output)) :do (setf (row-major-aref output i)
								      (copy-nested-array prototype))))
	    output))
      (if (and (not inverse) (loop :for d :across dimensions :always (= 0 (abs d))))
	  ;; in the case of a 0-size take of the array, append the remaining dimensions to the vector of zeroes
	  ;; passed to the function to create the empty output array
	  (make-array (append (array-to-list dimensions)
			      (loop :for i :from (length dimensions) :to (1- (rank input))
				 :collect (nth i (dims input))))
		      :element-type (if (and (< 0 (size input)) (arrayp (row-major-aref input 0)))
					(element-type (aref (row-major-aref input 0)))
					(assign-element-type (row-major-aref input 0))))
	  (let* ((isize (size input)) (irank (rank input))
		 (rdiff (- irank (length dimensions)))
		 (idims (make-array irank :element-type (list 'integer 0 isize)
				    :initial-contents (dims input))))
	    (if (< 0 rdiff)
		(setq dimensions (make-array irank :element-type (list 'integer 0 isize)
					     :initial-contents (loop :for x :below irank
								  :collect (if (< x rdiff) (aref dimensions x)
									       (if inverse 0 (aref idims x))))))
		(if (> 0 rdiff) (error "Too many subscripts (~w) for input array of rank ~w."
				       (length dimensions) irank)))
	    (if (and inverse (loop :for x :across dimensions :for y :in (dims input) :never (> y (abs x))))
		(make-array (loop :for i :below irank :collect 0))
		(let* ((odims (loop :for odim :across dimensions :for idim :across idims
				 :collect (if (not inverse) (abs odim) (- idim (abs odim)))))
		       (osize (reduce #'* odims))
		       (last-dim) (fill-element (apl-array-prototype input))
		       (id-factors (make-array irank :element-type 'fixnum))
		       (od-factors (make-array irank :element-type 'fixnum))
		       (output (make-array odims :element-type (if populator t (element-type input))
					   :initial-element (if (and (not populator)
								     (not (arrayp fill-element)))
								fill-element))))

		  ;; generate dimensional factors vectors for input and output
		  (loop :for dx :below irank
		     :do (let ((d (aref idims (- irank 1 dx))))
			   (setf (aref id-factors (- irank 1 dx))
				 (if (= 0 dx) 1 (* last-dim (aref id-factors (- irank dx))))
				 last-dim d)))

		  (loop :for d :in (reverse odims) :for dx :from 0
		     :do (setf (aref od-factors (- irank 1 dx))
			       (if (= 0 dx) 1 (* last-dim (aref od-factors (- irank dx))))
			       last-dim d))

		  ;; populate empty array elements if the array has a non-scalar prototype
		  (if populator (xdotimes output (i osize)
				  (setf (row-major-aref output i) (funcall populator)))
		      (if (arrayp fill-element)
			  (xdotimes output (i osize) (setf (row-major-aref output i)
							   (copy-nested-array fill-element)))))

		  (if (< 0 isize)
		      (if (< isize osize)
			  ;; choose shorter path depending on whether input or output are larger
			  (xdotimes output (i isize)
		      	    (let ((oindex 0) (remaining i) (valid t))
		      	      ;; calculate row-major offset for outer array dimensions
		      	      (loop :for i :from 0 :to (- irank 1) :while valid
		      		 :for dim :across dimensions :for id :across idims :for od :in odims
		      		 :for ifactor :across id-factors :for ofactor :across od-factors
		      		 :do (multiple-value-bind (index remainder) (floor remaining ifactor)
		      		       (let ((adj-index (- index (if inverse (if (> 0 dim) 0 dim)
		      						     (if (< 0 dim) 0 (+ dim id))))))
		      			 (if (< -1 adj-index od)
		      			     (progn (incf oindex (* ofactor adj-index))
		      				    (setq remaining remainder))
		      			     (setq valid nil)))))
		      	      (if valid (setf (row-major-aref output oindex) (row-major-aref input i)))))
			  (xdotimes output (o osize)
			    (let ((iindex 0) (remaining o) (valid t))
			      ;; calculate row-major offset for outer array dimensions
			      (loop :for i :from 0 :to (- irank 1) :while valid
				 :for dim :across dimensions :for id :across idims :for od :in odims
				 :for ifactor :across id-factors :for ofactor :across od-factors
				 :do (multiple-value-bind (index remainder) (floor remaining ofactor)
				       (let ((adj-index (+ index (if inverse (if (> 0 dim) 0 dim)
								     (if (< 0 dim) 0 (+ dim id))))))
					 (if (< -1 adj-index id)
					     (progn (incf iindex (* ifactor adj-index))
						    (setq remaining remainder))
					     (setq valid nil)))))
			      (if valid (setf (row-major-aref output o) (row-major-aref input iindex)))))))
		  output))))))

(defun catenate (a1 a2 axis)
  "Join two arrays along the specified axis."
  (let* ((rank1 (rank a1)) (rank2 (rank a2))
	 (dims1 (dims a1)) (dims2 (dims a2))
	 (max-rank (max rank1 rank2)) (uneven (/= rank1 rank2))
	 (lower-rank (if uneven (if (< rank1 rank2) 0 1)))
	 (offset (if (and uneven (< rank1 rank2)) 1 (nth axis (dims a1))))
	 (output-type (type-in-common (if (arrayp a1) (element-type a1) (assign-element-type a1))
				      (if (arrayp a2) (element-type a2) (assign-element-type a2))))
	 (ax1-len (if (and uneven (= 0 lower-rank)) 1 (or (nth axis dims1) 1)))
	 (ax2-len (if (and uneven (= 1 lower-rank)) 1 (or (nth axis dims2) 1)))
	 (increment (reduce #'* (nthcdr (1+ axis) (if (and uneven (= 0 lower-rank))
						      dims2 dims1))))
	 (vset1 (* increment ax1-len)) (vset2 (* increment ax2-len))
	 (catax-length) (out-vset))
    (if (and (> axis (1- max-rank))
	     (not (and (= 0 axis max-rank))))
	(error "Invalid axis (~a) for array with ~a dimensions." (1+ axis) max-rank))
    (if (loop :for a :in (list a1 a2) :always (= 0 (rank a)))
	(make-array 2 :element-type output-type :initial-contents (mapcar #'disclose (list a1 a2)))
	;; find the shape of the output using the higher-rank array as reference
	(let ((output (make-array (destructuring-bind (ref-array second-array)
				      (funcall (if (>= rank1 rank2) #'identity #'reverse)
					       (list a1 a2))
				    (if (and uneven (arrayp second-array)
					     (< 1 (abs (- rank1 rank2))))
					(error "Catenated arrays must be at most one rank apart.")
					;; iterate through the reference array dimensions
					;; and increment the axis dimension by one (if uneven) or the
					;; axis dimension of the other array if both are the same rank
					(loop :for i :from 0 :for rdim :in (dims ref-array)
					   :collect (if (= i axis)
							(setq catax-length
							      (+ rdim (if uneven
									  1 (nth i (dims second-array)))))
							(if (or (not (dims second-array))
								(= rdim (nth (- i (if (and uneven (> i axis))
										      1 0))
									     (dims second-array))))
							    rdim (error "Mismatched array dimensions."))))))
				  :element-type output-type)))
	  (setq out-vset (* increment catax-length))
	  
	  (xdotimes output (i (size output))
	    (multiple-value-bind (sets sremainder) (floor i out-vset)
	      (let ((vix (nth-value 1 (floor sremainder out-vset))))
	  	(let* ((first-input (< vix vset1))
	  	       (input (if first-input a1 a2))
	  	       (input-vset (if first-input vset1 vset2))
	  	       (offset (- (if first-input 0 vset1))))
	  	  (setf (row-major-aref output i)
	  		(if (not (arrayp input))
	  		    input (row-major-aref input (+ vix offset (* sets input-vset)))))))))
	  output))))

(defun laminate (a1 a2 axis)
  "Join the two arrays along a new axis inserted before the specified axis, the new axis having a length of 2."
  (flet ((rotate-right (n l)
	   "Rotate an array n units to the right."
	   (funcall (lambda (n l) (append (nthcdr n l) (butlast l (- (length l) n))))
		    (- (length l) n) l)))
    (let* ((permute-dims (alexandria:iota (1+ (rank a1))))
	   (pa1 (if (not (is-unitary a1))
		    (aops:permute (rotate-right axis permute-dims)
				  (array-promote a1))))
	   (pa2 (if (not (is-unitary a2))
		    (aops:permute (rotate-right axis permute-dims)
				  (array-promote a2)))))
      ;; a 1-element array argument to laminate is scaled to
      ;; match the other array's dimensions
      (catenate (or pa1 a1) (or pa2 a2)	axis))))

(defun expand (degrees input axis &key (compress-mode) (populator))
  "Expand an input array as per a vector of degrees, with the option to manifest zero values in the degree array as zeroes in the output in place of the original input values or to omit the corresponding values altogether if the :compress-mode option is used."
  (cond ((= 0 (size input))
	 (if compress-mode (error "An empty array cannot be compressed.")
	     (if (or (arrayp degrees)
		     (not (> 0 degrees)))
		 (error "An empty array can only be expanded to a single negative degree.")
		 (let ((output (make-array (abs degrees) :element-type (element-type input)
					   :initial-element (if (not populator) (apl-array-prototype input)))))
		   (if populator (xdotimes output (i (length output)) (setf (row-major-aref output i)
									    (funcall populator))))
		   output))))
	((and (or (not (arrayp input))
		  (= 0 (rank input)))
	      (not (arrayp degrees)))
	 (make-array degrees :element-type (type-of input) :initial-element (disclose input)))
	((and compress-mode (not (is-unitary input))
	      (and (arrayp degrees)
		   (/= (length degrees)
		       (nth axis (dims input)))))
	 (error "Attempting to replicate elements across array but ~a"
		"degrees are not equal to length of selected input axis."))
	((and (not compress-mode)
	      (and (arrayp input)
		   (< 1 (array-total-size input)))
	      (/= (or (and (arrayp degrees)
			   (loop :for degree :across degrees :when (< 0 degree)
			      :counting degree :into dcount :finally (return dcount)))
		      degrees
		      )
		  (nth axis (dims input))))
	 (error "Attempting to expand elements across array but ~a"
		"positive degrees are not equal to length of selected input axis."))
	(t (let* ((degrees (if (arrayp degrees) degrees
			       (make-array (nth axis (dims input)) :initial-element degrees)))
		  ;; TODO: is there a more elegant way to handle scalar degrees or input when both aren't scalar?
		  (c-degrees (make-array (length degrees) :element-type 'fixnum :initial-element 0))
		  (positive-index-list (if (not compress-mode)
					   (loop :for degree :below (length degrees)
					      :when (< 0 (aref degrees degree)) :collect degree)))
		  (positive-indices (if positive-index-list (make-array (length positive-index-list)
									:element-type 'fixnum
									:initial-contents positive-index-list)))
		  (section-size (reduce #'* (loop :for d :in (dims input) :for dx :from 0
		   	 		       :when (> dx axis) :collect d)))
		  (ex-dim))
	     (declare (dynamic-extent ex-dim))
	     (loop :for degree :across degrees :for dx :from 0
		:summing (max (abs degree) (if compress-mode 0 1))
		:into this-dim :do (setf (aref c-degrees dx) this-dim)
		:finally (setq ex-dim this-dim))
	     (let ((output (make-array (loop :for dim :in (or (dims input) '(1)) :for index :from 0
					  :collect (if (or (= index axis) (is-unitary input))
						       ex-dim dim))
				       :element-type (if (arrayp input)
							 (element-type input)
							 (assign-element-type input))
				       :initial-element (apl-array-prototype input))))
	       ;; in compress-mode: degrees must = length of axis,
	       ;; zeroes are omitted from output, negatives add zeroes
	       ;; otherwise: zeroes pass through, negatives add zeroes, degrees>0 must = length of axis
	       (if (is-unitary input)
		   ;; if the input is a unitary value, just expand or replicate with that value
		   (let ((value (if (not (arrayp input)) input (row-major-aref input 0))))
		     (dotimes (degree (length degrees))
		       (let ((this-degree (aref degrees degree)))
			 (xdotimes output (ix this-degree)
			   (setf (aref output (+ ix (if (= 0 degree) 0 (aref c-degrees (1- degree)))))
				 value)))))
		   (xdotimes output (i (size input))
		     (let* ((iseg (if (< 1 section-size) section-size (nth axis (dims input))))
			    (oseg (floor i iseg)) (ivix (mod i iseg))
			    (sub-index (if (= 1 section-size) ivix oseg))
		    	    (dx (if compress-mode sub-index (aref positive-indices sub-index)))
		   	    (offset (if (= 0 dx) 0 (aref c-degrees (1- dx)))))
		       (dotimes (d (aref degrees dx))
		       	 (setf (row-major-aref output (+ (if (< 1 section-size) ivix (* ex-dim oseg))
							 (* d section-size) (* offset section-size)))
		       	       (row-major-aref input i))))))
	       output)))))

(defun partitioned-enclose (positions input axis)
  "Enclose parts of an input array partitioned according to the 'positions' argument."
  (let* ((intervals) (offset 0) (input-offset 0)
	 (input (enclose-atom input)) (positions (enclose-atom positions))
	 (idims (dims input)) (axis-size (nth axis idims)))
    (declare (dynamic-extent intervals offset input-offset axis-size))
    ;; a scalar position argument is extended to the length of the input's first dimension
    (dotimes (i (if (is-unitary positions) (first idims) (length positions)))
      (let ((p (if (is-unitary positions) (get-first-or-disclose positions)
		   (aref positions i))))
	(if (= 0 p) (progn (if intervals (incf (first intervals)) (incf offset))
			   (incf input-offset))
	    (progn (setq intervals (append (loop :for i :below p :collect 0) intervals))
		   (if (> axis-size input-offset)
		       (progn (incf input-offset) (incf (first intervals))))))))
    (if (>= axis-size input-offset)
	(incf (first intervals) (- axis-size input-offset))
	(error "Size of partitions exceeds size of input array on axis ~w." axis))

    (let* ((input-offset 0) (intervals (reverse intervals))
	   (icoords (loop :for i :below (rank input) :collect 0))
	   (last-indim (first (last (dims input))))
	   (section-size (reduce #'* (loop :for d :in (dims input) :for dx :from 0
		   	 		:when (> dx axis) :collect d)))
	   (output (make-array (length intervals)
			       :initial-contents (loop :for intv :in intervals
						    :collect (make-array
							      (loop :for dim :in idims :for dx :from 0
								 :collect (if (= dx axis) intv dim))
							      :element-type (if (= 0 intv)
										t (element-type input)))))))
      (dotimes (oix (size output))
	(let* ((item (aref output oix))
	       (last-idim (first (last (dims item))))
	       (iseg (if (< 1 section-size) section-size (nth axis (dims input)))))
	  (dotimes (i (size item))
	    (let ((oseg (floor i iseg)) (ivix (mod i iseg)))
	      (setf (row-major-aref item i)
	       	    (row-major-aref input (if (= 1 section-size)
					      (+ offset input-offset (mod i last-idim)
						 (* last-indim (floor i last-idim)))
					      (+ ivix (* iseg (+ offset input-offset oseg))))))))
	  (incf input-offset (nth axis (dims item)))))
      output)))

(defun partition-array (positions input axis)
  "Split an array into an array of vectors divided according to an array of positions."
  (if (not (arrayp positions))
      (if (< 1 (array-total-size input))
	  (vector input)
	  (error "Rank error."))
      (let ((r-indices) (r-intervals) (indices) (intervals)
	    (interval-size 0)
	    (current-interval -1)
	    (partitions 0)
	    (idims (dims input))
	    (arank (rank input)))
	(declare (dynamic-extent r-indices r-intervals indices intervals
				 interval-size current-interval partitions))
	;; find the index where each partition begins in the input array and the length of each partition
	(loop :for pos :across positions :for p :from 0
	   :do (if (/= 0 current-interval)
		   (incf interval-size))
	   :when (or (< current-interval pos)
		     (and (= 0 pos) (/= 0 current-interval)))
	   :do (setq r-indices (cons p r-indices)
		     r-intervals (if (rest r-indices) (cons interval-size r-intervals)))
	   (incf partitions (if (/= 0 pos) 1 0))
	   (setq current-interval pos interval-size 0))
	;; add the last entry to the intervals provided the positions list didn't have a 0 value at the end
	(if (/= 0 (aref positions (1- (length positions))))
	    (setq r-intervals (cons (- (length positions) (first r-indices))
				    r-intervals)))

	(if (/= (length r-indices) (length r-intervals))
	    (setq r-indices (rest r-indices)))
	;; collect the indices and intervals into lists the right way around, dropping indices with 0-length
	;; intervals corresponding to zeroes in the positions list
	(loop :for rint :in r-intervals :for rind :in r-indices :when (/= 0 rint)
	   :do (setq intervals (cons rint intervals)
		     indices (cons rind indices)))
	(let* ((out-dims (loop :for dim :in idims :for dx :below arank
			    :collect (if (= dx axis) partitions dim)))
	       (output (make-array out-dims))
	       (icoords (loop :for i :below (rank input) :collect 0))
	       (section-size (reduce #'* (loop :for d :in (dims input) :for dx :from 0
		   	 		    :when (> dx axis) :collect d)))

	       (ofactors (get-dimensional-factors out-dims))
	       (ifactors (get-dimensional-factors (dims input))))
	  (dotimes (i (size output))
	    (let* ((focus (mod (floor i section-size) partitions))
	  	   (this-index (nth focus indices))
	  	   (this-interval (nth focus intervals))
	  	   (out-array (make-array this-interval :element-type (element-type input))))
	      (dotimes (ix this-interval)
		(let* ((rest i) (input-index 0))
		  (loop :for of :in ofactors :for if :in ifactors :for fx :from 0
		     :do (multiple-value-bind (factor remaining) (floor rest of)
			   (setq rest remaining)
			   (incf input-index (* if (if (/= fx axis) factor (+ ix this-index))))))
		  (setf (aref out-array ix) (row-major-aref input input-index))))
	      (setf (row-major-aref output i) out-array)))
	  output))))

(defun enlist (input &optional internal output (output-length 0))
  "Create a vector containing all elements of the input array in ravel order, breaking down nested and multidimensional arrays."
  (if (or (not (arrayp input))
	  (= 1 (array-total-size input)))
      input (progn (dotimes (i (size input))
		     (let ((item (row-major-aref input i)))
		       (if (arrayp item) (multiple-value-bind (out new-length)
					     (enlist item t output output-length)
					   (setq output out output-length new-length))
			   (setq output (cons item output)
				 output-length (1+ output-length)))))
		   (if internal (values output output-length)
		       (make-array output-length :element-type (element-type input)
				   :initial-contents (reverse output))))))

(defun reshape-to-fit (input output-dims &key (populator))
  "Reshape an array into a given set of dimensions, truncating or repeating the elements in the array until the dimensions are satisfied if the new array's size is different from the old."
  (if (or (not (arrayp input))
	  (= 0 (rank input)))
      (let ((output (make-array output-dims :element-type (assign-element-type input)
				:initial-element (disclose (copy-nested-array input)))))
	(if (arrayp input)
	    (xdotimes output (i (1- (size output)))
	      (setf (row-major-aref output (1+ i))
		    (disclose (copy-nested-array input)))))
	output)
      (if (= 0 (length output-dims))
          (row-major-aref input 0)
          (let* ((input-length (array-total-size input))
                 (output-length (reduce #'* output-dims))
                 (output (make-array output-dims :element-type (if populator t (element-type input)))))
	    ;; TODO: optimization caused problems due to type uncertainty; solution?
	    ;; (optimize (safety 0) (speed 3))
	    (if (or populator (< 0 (size input)))
		(xdotimes output (index output-length)
		  (setf (row-major-aref output index)
			(disclose (if populator (funcall populator)
				      (let ((item (row-major-aref input (mod index input-length))))
					(if (not (arrayp item)) item (copy-nested-array item))))))))
            output))))

(defun near-realp (x)
  "Determine if the number is 'real enough' to be treated as such."
  (or (realp x)
      (> single-float-epsilon (abs (imagpart x)))))

(defun near-integerp (x)
  "Determing if a number is close enough to an integer to be treated as such."
  (or (integerp x)
      (and (near-realp x)
           (> single-float-epsilon
              (abs (- (realpart x)
                      (fround (realpart x))))))))

(defun isprfact (n)
  "Recursive factorial-computing function. Based on P. Luschny's code."
  (let ((p 1) (r 1) (NN 1) (log2n (floor (log n 2)))
        (h 0) (shift 0) (high 1) (len 0))
    (declare (dynamic-extent p r NN h shift high len))
    (labels ((prod (n)
               (declare (fixnum n))
               (let ((m (ash n -1)))
                 (cond ((= m 0) (incf NN 2))
                       ((= n 2) (* (incf NN 2) (incf NN 2)))
                       (t (* (prod (- n m)) (prod m)))))))
      (loop :while (/= h n)
            :do (incf shift h)
                (setf h (ash n (- log2n)))
                (decf log2n)
                (setf len high)
                (setf high (if (oddp h) h (1- h)))
                (setf len (ash (- high len) -1))
                (cond ((> len 0)
                       (setf p (* p (prod len)))
                       (setf r (* r p)))))
      (ash r shift))))

(defun gamma (c)
  "Gamma function using Lanczos approximation"
  (if (< (realpart c) 0.5)
      (/ pi (* (sin (* pi c)) (gamma (- 1 c))))
      (let ((z c)
            (z1 (+ 5.5 c))
            (z2 (+ 0.5 c))
            ;; Lanczos approximation coefficients
            (p0 1.000000000190015)
            (p1 76.18009172947146 )
            (p2 -86.50532032941677)
            (p3 24.01409824083091)
            (p4 -1.231739572450155)
            (p5 1.208650973866179E-3)
            (p6 -5.395239384953E-6))
        (* (/ (sqrt (* 2.0 pi)) z)
           (+ p0 (/ p1 (+ z 1.0))
              (/ p2 (+ z 2.0))
              (/ p3 (+ z 3.0))
              (/ p4 (+ z 4.0))
              (/ p5 (+ z 5.0))
              (/ p6 (+ z 6.0)))
           (expt z1 z2)
           (exp (- z1))))))

(defun sprfact (n)
  "Top-level factorial-computing function."
  (if (near-integerp n)
      (isprfact (round (realpart n)))
      (gamma (+ n 1))))

(defun ibinomial (n k)
  "Find a binomial for integer parameters."
  (let ((k (min k (- n k)))
        (nom 1)
        (denom 1)
        (primes '(2 3 5 7 11 13 17 19)))
    (loop :for i :from 0 :below k
       :do (progn (setf nom (* nom (- n i))
			denom (* denom (1+ i)))
		  (when (> i 0)
                    (loop :for p :in primes
                       :if (zerop (mod i p))
                       :do (setf nom (/ nom p)
                                 denom (/ denom p)))))
       :finally (return (/ nom denom)))))

(defun binomial (n k)
  "Generalized binomial function. For complex and fractional numbers uses Gamma function."
  (if (and (near-integerp n) (near-integerp k))
      (let* ((a (round (realpart k)))
             (b (round (realpart n)))
             (how (+ (if (< a 0) 4 0) (if (< b 0) 2 0) (if (< b a) 1 0))))
        ;; (format t "~d ~d ~d ~%" a b how)
        (case how
          (0 (ibinomial b a))
          ((1 4 7) 0)
          (3 (* (if (oddp a) -1 1)
                (ibinomial (- a (1+ b))
                           a)))
          (6 (* (if (oddp (- b a)) -1 1)
                (ibinomial (- (1+ a)) (- (1+ b)))))
          (otherwise (error "Invalid combination of binomial parameters."))))
      (/ (gamma (+ 1 n))
         (* (gamma (+ 1 k))
            (gamma (- n k -1))))))

(defun reduce-array (input function axis &optional last-axis window)
  "Reduce an array along by a given function along a given dimension, optionally with a window interval."
  (if (= 0 (rank input))
      (make-array nil :initial-element (funcall function (aref input)
						(aref input)))
      (let* ((odims (dims input))
	     (axis (or axis (if (not last-axis) 0 (max 0 (1- (rank input))))))
	     (rlen (nth axis odims))
	     (increment (reduce #'* (nthcdr (1+ axis) odims)))
	     (wsegment)
	     (output (make-array (loop :for dim :in odims :for dx :from 0
				    :when (/= dx axis) :collect dim
				    :when (and window (= dx axis))
				    :collect (setq wsegment (- dim (1- window)))))))
	(xdotimes output (i (size output))
	  (declare (optimize (safety 1)))
	  (let ((value))
	    (loop :for ix :from (1- (or window rlen)) :downto 0
	       :do (let ((item (row-major-aref
				input (+ (* ix increment)
					 (if window (* rlen (floor i wsegment))
					     (if (= 1 increment)
						 0 (* (floor i increment)
						      (- (* increment rlen) increment))))
					 (if (/= 1 increment) i
					     (if window (if (>= 1 (rank input))
							    i (mod i wsegment))
						 (* i rlen)))))))
		     (setq value (if (not value) item (funcall function (disclose value)
							       (disclose item))))))
	    (setf (row-major-aref output i) value)))
	(if (not (and (= 0 (rank output))
		      (= 0 (rank (aref output)))))
	    output (disclose output)))))

(defun array-inner-product (alpha omega function1 function2)
  "Find the inner product of two arrays with two functions."
  (let* ((adims (dims alpha)) (odims (dims omega)) (arank (rank alpha)) (orank (rank omega))
	 (asegment (first (last adims)))
	 (osegment (if (not (and asegment (first odims) (/= asegment (first odims))))
		       (first odims) (error "To find an inner product the last dimension of the left array ~w"
					    "must equal the first dimension of the right array.")))
	 (osize (size omega)) (ovectors (reduce #'* (rest odims)))
	 (oholder (if odims (if (and (= osegment osize) (= 1 (rank omega)))
				omega (make-array osegment :element-type (element-type omega)))))
	 (output (make-array (append (butlast adims) (rest odims)))))
    ;; TODO: can't parallelize yet
    (dotimes (x (size output))
      (let* ((avix (floor x ovectors))
	     (ovix (mod x ovectors))
	     (adisp (if adims (make-array asegment :displaced-to alpha :element-type (element-type alpha)
					  :displaced-index-offset (* asegment avix)))))
	(if (and osegment (not (and (= osegment osize) (= 1 (rank omega)))))
	    (dotimes (i osegment) (setf (aref oholder i) (row-major-aref omega (+ ovix (* i ovectors))))))
	(setf (row-major-aref output x)
	      (if (= 0 arank orank) (funcall function1 omega alpha)
		  (reduce-array (apply-scalar function1 (or oholder omega) (or adisp alpha))
				function2 0)))))
    (if (not (and (= 0 (rank output))
		  (= 0 (rank (aref output)))))
	output (disclose output))))

(defun array-outer-product (omega alpha function)
  "Find the outer product of two arrays with a function."
  (let* ((ascalar (if (= 0 (rank alpha)) (disclose alpha)))
	 (oscalar (if (= 0 (rank omega)) (disclose omega)))
	 (osize (size omega)) (adims (dims alpha)) (odims (dims omega))
	 (output (make-array (append adims odims))))
    (xdotimes output (x (size output))
      (setf (row-major-aref output x)
      	    (funcall function (or oscalar (disclose (row-major-aref omega (mod x osize))))
      		     (or ascalar (disclose (row-major-aref alpha (floor x osize)))))))
    (funcall (if (= 0 (rank output)) #'disclose #'identity)
	     output)))

(defun inverse-outer-product (input function right-original &optional left-original)
  "Find the inverse outer product of an array with a function and a given outer product argument."
  (let* ((original (or left-original right-original))
	 (interval (reduce #'* (dims original)))
	 (output (make-array (if left-original (last (dims input) (- (rank input) (rank original)))
				 (butlast (dims input) (rank original))))))
    (xdotimes output (x (size output))
      (let ((input-index (* x (if left-original 1 interval))))
	(setf (row-major-aref output x)
	      (nest (funcall function (disclose (row-major-aref input input-index))
			     (disclose (row-major-aref original 0)))))))
    (funcall (if (= 0 (rank output)) #'disclose #'identity)
	     output)))

(defun index-of (to-search set count-from)
  "Find occurrences of members of one set in an array and create a corresponding array with values equal to the indices of the found values in the search set, or one plus the maximum possible found item index if the item is not found in the search set."
  (let* ((original-set set)
	 (set (if (or (vectorp set) (= 0 (rank set)))
		  (disclose set)
		  (let ((vectors (reduce #'* (butlast (dims set))))
			(last-dim (first (last (dims set)))))
		    (make-array vectors :initial-contents
				(loop :for i :below vectors
				   :collect (make-array last-dim :element-type (element-type set)
							:displaced-to set
							:displaced-index-offset (* i last-dim)))))))
	 (to-search (if (or (vectorp original-set) (not (arrayp original-set)))
			(if (vectorp to-search)
			    to-search (vector (disclose to-search)))
			(if (= (first (last (dims set)))
			       (first (last (dims to-search))))
			    (let ((vectors (reduce #'* (butlast (dims to-search))))
				  (last-dim (first (last (dims to-search)))))
			      (make-array vectors :initial-contents
					  (loop :for i :below vectors
					     :collect (make-array last-dim :displaced-to to-search
								  :element-type (element-type to-search)
								  :displaced-index-offset
								  (* i last-dim)))))
			    (error "Mismatch between array shapes - the last axes of ~w"
				   "arguments to dyadic ⍳ must have the same length."))))
	 (to-find (if (vectorp set)
		      set (vector set)))
	 (maximum (+ count-from (length to-find)))
	 (results (make-array (dims to-search) :element-type 'number)))
    (xdotimes results (index (array-total-size results))
      (let* ((search-index (row-major-aref to-search index))
	     (found (position search-index to-find :test #'array-compare)))
	(setf (row-major-aref results index)
	      (if found (+ count-from found)
		  maximum))))
    ;; TODO: must be original disclose
    (disclose-unitary results)))

(defun alpha-compare (compare-by)
  "Compare the contents of a vector according to their positions in an array, as when comparing an array of letters by their positions in the alphabet."
  (lambda (item1 item2)
    (if (numberp item1)
	(or (characterp item2)
	    (if (= item1 item2)
		:equal (funcall compare-by item1 item2)))
	(if (characterp item1)
	    (if (characterp item2)
		(if (char= item1 item2)
		    :equal (funcall compare-by (char-code item1)
				    (char-code item2))))))))

(defun vector-grade (compare-by vector1 vector2 &optional index)
  "Compare two vectors by the values of each element, giving priority to elements proportional to their position in the array, as when comparing words by the alphabetical order of the letters."
  (let ((index (if index index 0)))
    (cond ((>= index (length vector1))
	   (not (>= index (length vector2))))
	  ((>= index (length vector2)) nil)
	  (t (let ((compared (funcall compare-by (aref vector1 index)
				      (aref vector2 index))))
	       (if (eq :equal compared)
		   (vector-grade compare-by vector1 vector2 (1+ index))
		   compared))))))

(defun grade (input count-from compare-by)
  "Grade an array, using vector grading if 1-dimensional or decomposing the array into vectors and comparing those if multidimensional."
  (if (is-unitary input)
      count-from (let* ((input (if (= 1 (rank input))
				   input (aops:split input 1)))
			(input-length (length input))
			(vector (make-array input-length))
			(graded-array (make-array input-length
						  :element-type (list 'integer 0 input-length))))
		   (xdotimes graded-array (i input-length) (setf (aref graded-array i) (+ i count-from)))
		   (xdotimes vector (vix (length vector))
		     (setf (aref vector vix) (if (and (arrayp (aref input vix))
						      (< 1 (rank (aref input vix))))
						 (grade (aref input vix)
							count-from compare-by)
						 (aref input vix))))
		   (stable-sort graded-array (lambda (1st 2nd)
					       (let ((val1 (aref vector (- 1st count-from)))
						     (val2 (aref vector (- 2nd count-from))))
						 (cond ((not (arrayp val1))
							(if (arrayp val2)
							    (funcall compare-by val1 (aref val2 0))
							    (let ((output (funcall compare-by val1 val2)))
							      (and output (not (eq :equal output))))))
						       ((not (arrayp val2))
							(funcall compare-by (aref val1 0) val2))
						       (t (vector-grade compare-by val1 val2))))))
		   graded-array)))

(defun array-grade (compare-by input)
  "Grade an array."
  (aops:each (lambda (item)
	       (let ((coords))
		 (across compare-by (lambda (found indices)
				      (if (char= found item)
					  (setq coords (loop :for i :in indices :collect i)))))
		 (make-array (length coords) :initial-contents (reverse coords))))
	     input))

(defun interval-index (items reference)
  "Find the locations of indices of an array between the indices of a reference array."
  (labels ((interval-compare (ref)
	     (lambda (oitem)
	       (let ((match 0))
		 (aops:each (lambda (item)
			      (if (funcall (alpha-compare #'>) oitem item)
				  (incf match)))
			    ref)
		 match))))
    (if (vectorp reference)
	;; TODO: original disclose needed here
	(let ((last-number))
	  (if (loop :for i :across reference :always (or (not last-number) (< last-number i))
		 :do (setq last-number i))
	      (disclose-unitary (aops:each (interval-compare reference) (enclose-atom items)))
	      (error "Values left argument to ⍸ must be in ascending order.")))
	(if (and (<= (rank reference) (rank items))
		 (loop :for x :in (reverse (rest (dims reference)))
		    :for y :in (reverse (dims items)) :always (= x y)))
	    (let* ((items (enclose-atom items))
		   (ref-cells (make-array (1- (rank reference))))
		   (sub-dims (- (rank items) (1- (rank reference))))
		   (sub-arrays (make-array (- (rank items) sub-dims))))
	      (dotimes (c (1- (rank reference))) (setf (aref ref-cells c) (1+ c)))
	      (setq ref-cells (re-enclose reference ref-cells))
	      (loop :for i :from sub-dims :to (1- (rank items)) :do (setf (aref sub-arrays (- i sub-dims)) i))
	      (setq sub-arrays (re-enclose items sub-arrays))
	      (aops:each (lambda (sub-array)
			   (let ((match 0))
			     (aops:each (lambda (ref)
					  (let* ((ref (if (vectorp ref)
							  ref (make-array (array-total-size ref)
									  :element-type (element-type ref)
									  :displaced-to ref)))
						 (sub-array (if (vectorp sub-array)
								sub-array
								(make-array (array-total-size sub-array)
									    :element-type
									    (element-type sub-array)
									    :displaced-to sub-array))))
					    (if (vector-grade (alpha-compare #'<) ref sub-array)
						(incf match))))
					ref-cells)
			     match))
			 sub-arrays))
	    (error "Rank of left argument may not be greater than rank of right argument.")))))

(defun find-array (input target)
  "Find instances of an array within a larger array."
  (let* ((source-dims (dims input))
	 (output (make-array source-dims :element-type 'bit :initial-element 0)))
    (if (not (arrayp target))
	(if (not (arrayp input))
	    (if (characterp input)
		(or (and (characterp target)
			 (if (char= target input) 1 0))
		    0)
		(if (= target input) 1 0))
	    (progn (across input (lambda (element coords)
				   (setf (apply #'aref output coords)
					 (if (funcall (if (eql 'character (element-type input))
							  #'char= #'=)
						      target element)
					     1 0))))
		   output))
	(let ((target-head (row-major-aref target 0))
	      (target-dims (append (if (< (rank target) (rank input))
				       (loop :for index :below (- (rank input) (rank target))
					  :collect 1))
				   (dims target))))
	  (loop :for match :in (let ((match-coords))
				 (across input (lambda (element coords)
						 (if (equal element target-head)
						     ;; make a copy of the coords list since (across) reuses it
						     (setq match-coords (cons (loop :for c :in coords :collect c)
									      match-coords)))))
				 match-coords)
	     :do (let ((target-index 0)
		       (target-matched t)
		       (target-displaced (make-array (array-total-size target) :displaced-to target
						     :element-type (element-type target)))
		       (element-list (loop :for i :below (rank input) :collect nil)))
		   (labels ((process-dims (starts extents limits count)
			      (if starts (let ((start (first starts))
					       (extent (first extents))
					       (limit (first limits)))
					   (setf (nth count element-list)
						 (iota (min extent (- limit start))
						       :start start))
					   (process-dims (rest starts) (rest extents)
							 (rest limits) (1+ count))))))
		     (process-dims match target-dims source-dims 0))
		   (across input (lambda (element coords)
				   (declare (ignore coords))
				   (if (and (< target-index (length target-displaced))
					    (not (equal element (aref target-displaced target-index))))
				       (setq target-matched nil))
				   (incf target-index))
			   :elements element-list)
		   ;; check the target index in case the elements in the searched array ran out
		   (if (and target-matched (= target-index (length target-displaced)))
		       (incf (apply #'aref output match)))))
	  output))))

(defun axes-to-indices (ic idims out-vector &optional if start)
  "Take a list of axes and assign the derived row-major indices to a vector with a fill point."
  (let* ((fic (first ic))
	 (if (or if (let ((dims idims))
		      (loop :while dims :collect (reduce #'* (setq dims (rest dims)))))))
	 (fif (first if))
	 (start (or start 0)) (at-index-end (not (rest idims))))
    (cond ((null fic) ;; a nil axis is elided; all indices on dimension traversed
	   (dotimes (i (first idims))
	     (if at-index-end (vector-push (+ start i) out-vector)
		 (axes-to-indices (rest ic) (rest idims) out-vector
				  (rest if) (+ start (* fif i))))))
	  ((integerp fic) ;; an integer axis designates a single index
	   (if at-index-end (vector-push (+ start fic) out-vector)
	       (axes-to-indices (rest ic) (rest idims) out-vector
				(rest if) (+ start (* fif fic)))))
	  ((vectorp fic) ;; a vector axis specifies the indices to be traversed
	   (loop :for i :across fic
	      :do (if at-index-end (vector-push (+ start i) out-vector)
		      (axes-to-indices (rest ic) (rest idims) out-vector
				       (rest if) (+ start (* fif i))))))
	  ((arrayp fic)
	   ;; a higher-dimensional array axis specifies indices to be traversed
	   ;; populating a new dimension of the output array
	   (dotimes (i (size fic))
	     (if at-index-end (vector-push (+ start (row-major-aref fic i)) out-vector)
		 (axes-to-indices (rest ic) (rest idims) out-vector (rest if)
				  (+ start (* fif  (row-major-aref fic i))))))))))

(defun choose (input indices &key (set) (set-by) (modify-input))
  "Select indices from an array and return them in an array shaped according to the requested indices, with the option to elide indices and perform an operation on the values at the indices instead of just fetching them and return the entire altered array."
  (let* ((idims (dims input)) (sdims (if set (dims set)))
	 ;; contents removed from 1-size arrays in the indices
	 (indices (loop :for i :in indices :collect (if (not (and (arrayp i) (= 1 (size i))))
							i (row-major-aref i 0))))
	 (index1 (first indices)) (naxes (< 1 (length indices)))
	 (odims (if naxes (loop :for i :in indices :for d :in idims :for s :from 0
			     :append (let ((len (or (and (null i) (list d))
						    (and (integerp i) nil)
						    (and (arrayp i) (dims i)))))
				       (if (and (not len) (not (integerp i)))
					   (error "Invalid index."))
				       ;; collect output dimensions according to indices;
				       ;; this is necessary even when setting values
				       ;; compatible with the input array in order
				       ;; to catch invalid indices
				       (if (and len sdims (or (< 1 (length len))
							      (/= (first len) (nth s sdims))))
					   (error "Invalid input."))
				       len))
		    (if (or set set-by) (dims input)
			(dims (or (first indices) input)))))
	 (rmi-type (list 'integer 0 (reduce #'* idims)))
	 (index-vector (if (and (arrayp index1) (not naxes))
			   (let ((reach-indexing)
				 (output (make-array (size index1) :element-type rmi-type)))
			     ;; if each element in the indices is valid, either as an integer
			     ;; or a vector, return the output, otherwise reach indexing is
			     ;; being performed so turn on its flag and return nil
			     (loop :for i :below (size index1) :while (not reach-indexing)
				:do (let ((ss (rmi-from-subscript-vector
					       input (disclose (row-major-aref index1 i)))))
				      (if ss (setf (aref output i) ss)
					  (setq reach-indexing t))))
			     (if (not reach-indexing) output))))
	 ;; create the vector of row-major indices
	 (rmindices (if naxes (make-array (reduce #'* odims) :element-type rmi-type :fill-pointer 0)
			(and indices (or index-vector
					 (if (not (arrayp index1)) (vector index1)
					     (make-array (size index1) :displaced-to index1
							 :element-type (element-type index1)))))))
	 ;; if there is a set-by function, there's no efficient way to derive the output type
	 ;; so just set it to t, otherwise use the most efficient common type
	 (set-type (if set (if set-by t (type-in-common (element-type input) (element-type set)))))
	 ;; create output array if 1. nothing is being set, this is just a retrieval operation, or
	 ;; 2. a scalar value is being set and its type is not compatible with the input
	 ;; or 3. the input and the items to set are in arrays of different types
	 (output (if (or set-by (and (or (not set)
					 ;; an output array is used if the types of the input and
					 ;; values to set are not compatible
					 (not modify-input)
					 ;; the array may be changed in place only if the modify-input
					 ;; parameter is not set
					 (not (or (eq t (element-type input))
						  (and (not (arrayp set)) (typep set (element-type input)))
						  (and (arrayp set) (not (eq t set-type))
						       (typep input set-type)))))
				     ;; if setting is not being done (i.e. values are being retrieved)
				     ;; and only one index is being fetched, an output array isn't needed
				     (or set (< 1 (size rmindices))
					 (and odims (< 1 (length indices))))))
		     (make-array (if set idims odims)
				 :element-type (if set-by t (or set-type (element-type input))))))
	 ;; the default set-by function just returns the second argument
	 (set-by (or set-by (if set (lambda (a b) (declare (ignore a)) b)))))
    ;; if multiple axes have been passed, populate the vector of row-major indices
    (if naxes (axes-to-indices indices idims rmindices))
    (if (or set set-by)
	(let ((pindices (if (and output indices) (make-array (size input) :initial-element 0))))
	  ;; if an output array is being used, the processed indices vector stores the indices
	  ;; of elements that have been processed so the ones that weren't can be assigned
	  ;; values from the input
	  (if indices (dotimes (o (length rmindices))
			(let ((i (aref rmindices o)))
			  (if (integerp i)
			      (let ((result (disclose (apply set-by (row-major-aref input i)
							     ;; apply the set-by function to an element
							     ;; of the set values if they're in an array
							     ;; or to the set value if it's scalar
							     (or (and (arrayp set) (< 0 (rank set))
								      (list (row-major-aref set o)))
								 (and set (list set)))))))
				(if output (setf (row-major-aref output i) result)
				    (setf (row-major-aref input i) result))
				(if pindices (setf (aref pindices i) 1)))
			      (if (vectorp i) ;; this clause can only be reached when reach indexing
				  ;; the set-by function is called on each pair of input
				  ;; and replacement values
				  (progn (setf (varef (or output input) i)
					       (funcall set-by (varef input i)
							(or (and (arrayp set) (row-major-aref set o))
							    set)))
					 (if pindices (setf (aref pindices (rmi-from-subscript-vector input i))
							    1)))))))
	      (xdotimes output (i (size input))
		;; if there are no indices the set-by function is to
		;; be run on all elements of the array
		(setf (row-major-aref output i)
		      (apply set-by (row-major-aref input i)
			     (and set (list set))))))
	  (if pindices (progn (xdotimes output (i (size input))
				(if (= 0 (aref pindices i))
				    (setf (row-major-aref output i)
					  (row-major-aref input i))))
			      output))
	  output)
	;; if a single index is specified, from the output, just retrieve its value
	(if (not output) (enclose (row-major-aref input (row-major-aref rmindices 0)))
	    (progn (xdotimes output (o (length rmindices))
		     (let ((i (aref rmindices o)))
		       (setf (row-major-aref output o)
			     (if (integerp i) (row-major-aref input i)
				 ;; the vectorp clause is only used when reach-indexing
				 (if (vectorp i)
				     (choose (disclose (varef input (disclose (aref i 0))))
					     (rest (array-to-list i))))))))
		   output)))))

(defun mix-arrays (axis input)
  "Combine an array of nested arrays into a higher-rank array, removing a layer of nesting."
  (if (or (not (arrayp input))
	  (and (= 1 (array-total-size input))
	       (not (arrayp (row-major-aref input 0)))))
      input
      (let* ((each-type) (type) (output)
	     (isize (size input)) (irank (rank input)) (total-size 0)
	     (input-vector (make-array (size input) :displaced-to input :element-type (element-type input)))
	     (each-interval (make-array (size input) :element-type 'fixnum))
	     (max-rank (loop :for i :across input-vector :maximizing (rank i)))
	     (orank (+ max-rank irank))
	     (out-dims-vector (make-array max-rank :element-type 'fixnum))
	     (ifactors (get-dimensional-factors (dims input)))
	     (ifactor-matrix (make-array (list isize (max 0 (1- max-rank))) :element-type 'fixnum))
	     (idims-holder (make-array max-rank :element-type 'fixnum :initial-element 0))
	     (odims-holder (make-array orank :element-type 'fixnum :fill-pointer 0))
	     (ofactors (make-array orank :element-type 'fixnum :initial-element 1)))

	(dotimes (ix isize)
	  (let* ((i (aref input-vector ix))
		 (this-size (size i)) (this-rank (rank i)))
	    (incf total-size this-size)
	    (if (= 0 ix) (setf (aref each-interval 0) this-size)
		(setf (aref each-interval ix) (+ this-size (aref each-interval (1- ix)))))
	    ;; find types and maximum dimensions for input sub-arrays
	    (loop :for d :in (dims i) :for dx :from 0
	       :do (let ((dmix (+ dx (- max-rank this-rank)))
			 (this-type (if (arrayp i) (element-type i) (type-of i))))
		     (setf (aref out-dims-vector dmix)
			   (max d (aref out-dims-vector dmix))
			   type (if (not type) this-type (type-in-common type this-type))
			   (aref idims-holder dmix) d)))
	    ;; calculate dimensional factors for the appropriate row of the factor matrix
	    (loop :for r :from (- this-rank 2) :downto 0 :for i :from 0
	       :do (setf (row-major-aref ifactor-matrix (+ r (* ix (1- max-rank))))
		     	 (* (if (= i 0) 1 (row-major-aref ifactor-matrix (+ 1 r (* ix (1- max-rank)))))
		     	    (aref idims-holder (- max-rank 1 i)))))))

	;; populate the vector of output dimensions according to the axis
	(let ((axis-index 0)
	      (outer (dims input)))
	  (dotimes (odix (1+ irank))
	    (if (and axis (= odix (- axis (min 1 axis-index))))
		(progn (incf axis-index)
		       (loop :for i :across out-dims-vector :do (vector-push i odims-holder)))
		(if (> irank (- odix axis-index))
		    (vector-push (nth (- odix axis-index) outer) odims-holder)))))

	;; populate vector of output dimensional factors
	(loop :for i :from (- orank 2) :downto 0
	   :do (setf (aref ofactors i) (* (aref odims-holder (1+ i))
					  (if (= i (- orank 2))
					      1 (aref ofactors (1+ i))))))

	;; create output array
	(setq output (make-array (loop :for d :across odims-holder :collect d)
				 :element-type (or type t) :initial-element
				 (if (eql 'character type) #\ (coerce 0 (or type t)))))

	;; rotate output array dimensional factors according to axis
	;; algorithm: rotate last (vector length - x) elements y times
	;; x = axis, y = output rank - axis
	(if (/= axis irank)
	    (if (= 0 axis) (rotate ofactors (- irank axis))
		(let ((to-rotate (make-array (- orank axis) :displaced-to ofactors
					     :element-type 'fixnum :displaced-index-offset axis)))
		  (rotate to-rotate (- irank axis)))))
	
	(xdotimes output (i total-size)
	  (if (= 0 max-rank)
	      (setf (row-major-aref output i) (disclose (row-major-aref input i)))
	      (let ((offset 0) (input-index 0) (out-index 0) (this-rank)
		    (input-element) (this-index) (remaining) (input-rank-delta))
		(loop :for s :across each-interval :for sx :from 0 :while (<= s i)
		   :do (setq offset s
	    		     input-index (1+ sx)))
		(setq this-index (- i offset)
		      remaining input-index
		      this-rank (rank (aref input-vector input-index))
		      input-rank-delta (- max-rank this-rank)
		      input-element (aref input-vector input-index))

		;; calculate row-major offset for outer array dimensions
		(loop :for r :from 0 :to (- irank 2)
		   :for ifactor :in ifactors :for ofactor :across ofactors
		   :do (multiple-value-bind (index remainder) (floor remaining ifactor)
			 (incf out-index (* index ofactor))
			 (setq remaining remainder)))

		(incf out-index (* remaining (aref ofactors (1- irank))))
		(setq remaining this-index)

		(if (not (arrayp input-element))
		    (setf (row-major-aref output out-index) input-element)
		    (progn
		      (if (= 0 (1- this-rank))
			  ;; if the inner array/object is scalar, just assign its disclosed value
			  ;; to the proper location in the output array
			  (incf out-index (* this-index (aref ofactors (1- orank))))
			  ;; else, calculate row-major index accoridng to inner array dimensions
			  (loop :for r :below (1- this-rank)
			     :for i :from irank :to (1- orank) :for ix :from 0
			     :do (multiple-value-bind (index remainder)
				     (floor remaining (row-major-aref ifactor-matrix
								      (+ ix (* input-index (1- max-rank)))))
				   (incf out-index (* index (aref ofactors (+ i input-rank-delta))))
				   (if (= r (- this-rank 2))
				       (incf out-index (* remainder (aref ofactors (+ 1 i input-rank-delta))))
				       (setf remaining remainder)))))
		      (setf (row-major-aref output out-index)
			    (row-major-aref input-element this-index)))))))
	output)))

(defun split-array (input &optional axis)
  "Split an array into a set of sub-arrays."
  (if (> 2 (rank input))
      (if (not (arrayp input))
	  input (make-array nil :initial-element input))
      (let* ((irank (rank input)) (idims (dims input)) (last-dim)
	     (axis (if axis axis (1- irank))) (axis-dim (nth axis idims))
	     (ocoords (loop :for i :below (1- irank) :collect 0))
	     (odims (loop :for dim :in idims :for dx :from 0 :when (not (= dx axis)) :collect dim))
	     (input-factors (make-array irank :element-type 'fixnum))
	     (output-factors (make-array (1- irank) :element-type 'fixnum))
	     (output (make-array odims)))
	(loop :for d :in (reverse idims) :for dx :from 0
	   :do (setf (aref input-factors (- irank 1 dx))
		     (if (= 0 dx) 1 (* last-dim (aref input-factors (- irank dx))))
		     last-dim d))
	(loop :for d :in (reverse odims) :for dx :from 0
	   :do (setf (aref output-factors (- irank 2 dx))
		     (if (= 0 dx) 1 (* last-dim (aref output-factors (- irank 1 dx))))
		     last-dim d))
	(xdotimes output (o (size output))
	  (setf (row-major-aref output o) (make-array axis-dim :element-type (element-type input))))
	
	(xdotimes input (i (size input))
	  (let ((oindex 0) (iindex) (remaining i) (odix 0))
	    ;; calculate row-major offset for outer array dimensions
	    (loop :for r :from 0 :to (- irank 1) :for ifactor :across input-factors :for ix :from 0
	       :do (multiple-value-bind (index remainder) (floor remaining ifactor)
		     (if (= axis ix) (setq iindex index)
			 (incf oindex (* index (row-major-aref output-factors (1- (incf odix))))))
		     (setq remaining remainder)))
	    (setf (row-major-aref (row-major-aref output oindex) iindex)
		  (row-major-aref input i))))
	output)))

(defun ravel (count-from input &optional axes)
  "Produce a vector from the elements of a multidimensional array."
  (if (and (not axes) (> 2 (rank input)))
      (if (= 1 (rank input))
	  input (make-array 1 :initial-element input :element-type (assign-element-type input)))
      (if axes (cond ((and (numberp (first axes))
			   (not (integerp (first axes))))
		      (make-array (if (not (first axes))
				      (append (dims input)
					      '(1))
				      (funcall (lambda (lst index)
						 (if (= 0 index)
						     (setq lst (cons 1 lst))
						     (push 1 (cdr (nthcdr (1- index) lst))))
						 lst)
					       (dims input)
					       (- (ceiling (first axes))
						  count-from)))
				  :element-type (element-type input) :displaced-to (copy-nested-array input)))
		     ;; TODO: fix these clauses
		     ;; ((and (< 1 (length (first axes)))
		     ;; 	 (or (> 0 (aref (first axes) 0))
		     ;; 	     (> (aref (first axes) (1- (length (first axes))))
		     ;; 		(rank input))
		     ;; 	     (not (loop :for index :from 1 :to (1- (length (first axes)))
		     ;; 		     :always (= (aref (first axes) index)
		     ;; 				(1+ (aref (first axes) (1- index))))))))
		     ;;  (error (concatenate 'string "Dimension indices must be consecutive and within "
		     ;; 			"the array's number of dimensions.")))
		     ((or (integerp (first axes))
			  (< 0 (length (first axes))))
		      ;; TODO: eliminate consing here
		      (let* ((axl (if (not (arrayp (first axes)))
				      (list (first axes))
				      (mapcar (lambda (item) (- item count-from))
					      (array-to-list (first axes)))))
			     (collapsed (apply #'* (mapcar (lambda (index) (nth index (dims input)))
							   axl))))
			(labels ((dproc (dms &optional index output)
				   (let ((index (if index index 0)))
				     (if (not dms)
					 (reverse output)
					 (dproc (if (= index (first axl))
						    (nthcdr (length axl) dms)
						    (rest dms))
						(1+ index)
						(cons (if (= index (first axl))
							  collapsed (first dms))
						      output))))))
			  (make-array (dproc (dims input))
				      :element-type (element-type input)
				      :displaced-to (copy-nested-array input))))))
	  ;; TODO: this generates an array of type vector, not simple-array since it's displaced, is this a
	  ;; problem? Look into it
	  (make-array (array-total-size input) :element-type (element-type input)
		      :displaced-to (copy-nested-array input)))))

(defun re-enclose (input axes)
  "Convert an array into a set of sub-arrays within a larger array. The dimensions of the containing array and the sub-arrays will be some combination of the dimensions of the original array. For example, a 2 x 3 x 4 array may be composed into a 3-element vector containing 2 x 4 dimensional arrays."
  (labels ((make-enclosure (inner-dims type dimensions)
	     (loop :for d :below (first dimensions)
		:collect (if (= 1 (length dimensions))
			     (make-array inner-dims :element-type type)
			     (make-enclosure inner-dims type (rest dimensions))))))
    (cond ((= 1 (length axes))
	   ;; if there is only one axis just split the array
	   (if (>= (aref axes 0) (rank input))
	       (error "Invalid axis ~w for array of rank ~w." (aref axes 0) (rank input))
	       (let* ((axis (aref axes 0))
		      (output (make-array (loop :for d :in (dims input) :for dx :from 0
					     :when (/= dx axis) :collect d))))
		 (xdotimes output (o (size output))
		   (setf (row-major-aref output o) (make-array (nth axis (dims input))
							       :element-type (element-type input))))
		 (across input (lambda (elem coords)
				  (let ((out-sub-array (loop :for c :in coords :for cx :from 0
							  :when (/= cx axis) :collect c))
					(out-coords (nth axis coords)))
				    (setf (aref (apply #'aref output out-sub-array) out-coords) elem))))
		 output)))
	  ((not (apply #'< (array-to-list axes)))
	   (error "Elements in an axis argument to the enclose function must be in ascending order."))
	  ((= (length axes) (rank input))
	   ;; if the number of axes is the same as the input's rank, just pass it back
	   input)
	  (t (let* ((outer-dims) (inner-dims)
		    (input-dims (dims input))
		    (axis-list (array-to-list axes)))
	       ;; otherwise, start by separating the dimensions of the original array into sets of dimensions
	       ;; for the output array and each of its enclosed arrays
	       (dotimes (axis (rank input))
		 (if (find axis axis-list) (setq inner-dims (cons axis inner-dims))
		     (setq outer-dims (cons axis outer-dims))))
	       (setq inner-dims (reverse inner-dims)
		     outer-dims (reverse outer-dims))
	       ;; create a blank array of the outer dimensions containing blank arrays of the inner dimensions
	       (let* ((ocoords (loop :for d :in outer-dims :collect (nth d input-dims)))
		      (icoords (loop :for d :in inner-dims :collect (nth d input-dims)))
		      (output (make-array ocoords :initial-contents
					  (make-enclosure icoords (element-type input)
							  ocoords)))
		      (infactors (get-dimensional-factors (dims input)))
		      (inner-factors (get-dimensional-factors icoords))
		      (outer-factors (get-dimensional-factors ocoords)))
		 ;; iterate through the original array and for each element, apply the same separation
		 ;; to their coordinates that was done to the original array's dimensions and apply the two sets
		 ;; of coordinates to set each value in the nested output arrays to the corresponding values in
		 ;; the original array
		 (xdotimes (row-major-aref output 0) (i (size input))
		   (let* ((rest i) (inner-index 0) (inner-dx 0) (outer-index 0) (outer-dx 0))
		     (loop :for f :in infactors :for fx :from 0
			:do (multiple-value-bind (factor remaining) (floor rest f)
			      (setq rest remaining)
			      (if (loop :for a :across axes :never (= fx a))
				  (progn (incf outer-index (* factor (nth outer-dx outer-factors)))
					 (incf outer-dx))
				  (progn (incf inner-index (* factor (nth inner-dx inner-factors)))
					 (incf inner-dx)))))
		     (setf (row-major-aref (row-major-aref output outer-index) inner-index)
		 	   (row-major-aref input i))))
		 output))))))

(defun turn (input axis &optional degrees)
  "Scan a function across an array along a given axis. Used to implement the [\ scan] operator with an option for inversion when used with the [⍣ power] operator taking a negative right operand."
  (if (and degrees (not (or (is-unitary degrees)
			    (and (= (rank degrees) (1- (rank input)))
				 (loop :for dd :in (dims degrees)
				    :for id :in (loop :for d :in (dims input) :for dx :from 0
						   :when (/= dx axis) :collect d)
				    :always (= dd id))))))
      (error "Invalid degree array; degree array must have same dimensions as input array excluding the ~a"
	     "axis along which the input array is to be rotated."))
  (if (not (arrayp input))
      input (let* ((idims (dims input))
		   (rlen (nth axis idims))
		   (increment (reduce #'* (nthcdr (1+ axis) idims)))
		   (vset-size (* increment (nth axis idims)))
		   (output (make-array idims :element-type (element-type input)))
		   (adjuster (if degrees #'identity (lambda (x) (abs (- x (1- rlen)))))))
	      (dotimes (i (size input))
		(declare (optimize (safety 1)))
		(let ((vindex (funcall adjuster
				       (mod (- (floor i increment)
					       (if (integerp degrees)
					       	   degrees (if (arrayp degrees)
					       		       (row-major-aref
					       			degrees
					       			(+ (mod i increment)
					       			   (* increment (floor i vset-size))))
					       		       0)))
					    rlen))))
 		  (setf (row-major-aref output (+ (mod i increment) (* increment vindex)
					          (* vset-size (floor i vset-size))))
			(row-major-aref input i))))
	      output)))

(defun invert-matrix (in-matrix)
  "Find the inverse of a square matrix."
  (let ((dim (array-dimension in-matrix 0))   ;; dimension of matrix
	(det 1)                               ;; determinant of matrix
	(l)                                   ;; permutation vector
	(m)                                   ;; permutation vector
	(temp 0)
	(out-matrix (make-array (dims in-matrix))))

    (if (not (equal dim (array-dimension in-matrix 1)))
	(error "invert-matrix () - matrix not square"))

    ;; (if (not (equal (array-dimensions in-matrix)
    ;;                 (array-dimensions out-matrix)))
    ;;     (error "invert-matrix () - matrices not of the same size"))

    ;; copy in-matrix to out-matrix if they are not the same
    (when (not (equal in-matrix out-matrix))
      (do ((i 0 (1+ i)))
	  ((>= i dim))    
	(do ((j 0 (1+ j)))
	    ((>= j dim)) 
	  (setf (aref out-matrix i j) (aref in-matrix i j)))))

    ;; allocate permutation vectors for l and m, with the 
    ;; same origin as the matrix
    (setf l (make-array `(,dim)))
    (setf m (make-array `(,dim)))

    (do ((k 0 (1+ k))
	 (biga 0)
	 (recip-biga 0))
	((>= k dim))

      (setf (aref l k) k)
      (setf (aref m k) k)
      (setf biga (aref out-matrix k k))

      ;; find the biggest element in the submatrix
      (do ((i k (1+ i)))
	  ((>= i dim))    
	(do ((j k (1+ j)))
	    ((>= j dim)) 
	  (when (> (abs (aref out-matrix i j)) (abs biga))
	    (setf biga (aref out-matrix i j))
	    (setf (aref l k) i)
	    (setf (aref m k) j))))

      ;; interchange rows
      (if (> (aref l k) k)
	  (do ((j 0 (1+ j))
	       (i (aref l k)))
	      ((>= j dim)) 
	    (setf temp (- (aref out-matrix k j)))
	    (setf (aref out-matrix k j) (aref out-matrix i j))
	    (setf (aref out-matrix i j) temp)))

      ;; interchange columns 
      (if (> (aref m k) k)
	  (do ((i 0 (1+ i))
	       (j (aref m k)))
	      ((>= i dim)) 
	    (setf temp (- (aref out-matrix i k)))
	    (setf (aref out-matrix i k) (aref out-matrix i j))
	    (setf (aref out-matrix i j) temp)))

      ;; divide column by minus pivot (value of pivot 
      ;; element is in biga)
      (if (equalp biga 0) 
	  (return-from invert-matrix 0))
      (setf recip-biga (/ 1 biga))
      (do ((i 0 (1+ i)))
	  ((>= i dim)) 
	(if (not (equal i k))
	    (setf (aref out-matrix i k) 
		  (* (aref out-matrix i k) (- recip-biga)))))

      ;; reduce matrix
      (do ((i 0 (1+ i)))
	  ((>= i dim)) 
	(when (not (equal i k))
	  (setf temp (aref out-matrix i k))
	  (do ((j 0 (1+ j)))
	      ((>= j dim)) 
	    (if (not (equal j k))
		(incf (aref out-matrix i j) 
		      (* temp (aref out-matrix k j)))))))

      ;; divide row by pivot
      (do ((j 0 (1+ j)))
	  ((>= j dim)) 
	(if (not (equal j k))
	    (setf (aref out-matrix k j)
		  (* (aref out-matrix k j) recip-biga))))

      (setf det (* det biga)) ;; product of pivots
      (setf (aref out-matrix k k) recip-biga)) ;; k loop

    ;; final row & column interchanges
    (do ((k (1- dim) (1- k)))
	((< k 0))
      (if (> (aref l k) k)
	  (do ((j 0 (1+ j))
	       (i (aref l k)))
	      ((>= j dim))
	    (setf temp (aref out-matrix j k))
	    (setf (aref out-matrix j k) 
		  (- (aref out-matrix j i)))
	    (setf (aref out-matrix j i) temp)))
      (if (> (aref m k) k)
	  (do ((i 0 (1+ i))
	       (j (aref m k)))
	      ((>= i dim))
	    (setf temp (aref out-matrix k i))
	    (setf (aref out-matrix k i) 
		  (- (aref out-matrix j i)))
	    (setf (aref out-matrix j i) temp))))
    det ;; return determinant
    out-matrix))

(defun stencil (input process window-dims movement)
  "Apply a given function to sub-arrays of an array with specified dimensions sampled according to a given pattern of movement across the array."
  (let* ((irank (rank input))
	 (idims (apply #'vector (dims input)))
	 (wrank (length window-dims))
	 (output-dims (loop :for dim :below (length window-dims)
			 :collect (ceiling (- (/ (aref idims dim) (aref movement dim))
					      (if (and (evenp (aref window-dims dim))
						       (or (= 1 (aref movement dim))
							   (oddp (aref idims dim))))
						  1 0)))))
	 (in-factors (make-array (rank input) :element-type 'fixnum))
	 (out-factors (make-array (rank input) :element-type 'fixnum))
	 (win-factors (make-array wrank :element-type 'fixnum))
	 (output (make-array output-dims))
	 (ref-coords (loop :for d :across window-dims :collect 0))
	 (acoords (loop :for d :in output-dims :collect 0))
	 (last-dim))
    
    ;; generate dimensional factors vector for window dimensions
    (loop :for dx :below (length window-dims)
       :do (let ((d (aref window-dims (- wrank 1 dx))))
	     (setf (aref win-factors (- wrank 1 dx))
		   (if (= 0 dx) 1 (* last-dim (aref win-factors (- wrank dx))))
		   last-dim d)))
    
    ;; generate dimensional factors vector for input
    (loop :for dx :below (rank input)
       :do (let ((d (aref idims (- irank 1 dx))))
	     (setf (aref in-factors (- irank 1 dx))
		   (if (= 0 dx) 1 (* last-dim (aref in-factors (- irank dx))))
		   last-dim d)))
    
    ;; generate dimensional factors vector for output
    (loop :for d :in output-dims :for dx :from 0
       :do (setf (aref out-factors (- irank 1 dx))
		 (if (= 0 dx) 1 (* last-dim (aref out-factors (- irank dx))))
		 last-dim d))

    ;; (print (list :ao in-factors out-factors win-factors window-dims movement out-factors))
    
    (xdotimes output (o (size output))
      (let* ((acoords (make-array (rank input) :element-type 'fixnum))
	     (oindices (let ((remaining o))
			 (loop :for if :across in-factors :for ix :from 0
			    :for of :across out-factors :for melem :across movement
			    :collect (multiple-value-bind (index remainder)
					 ;; TODO: is this correct?
					 (floor remaining (if (= 1 (rank input)) if (* melem of)))
				       (setf remaining remainder)
				       index))))
	     (window (make-array (array-to-list window-dims) :element-type (element-type input))))
	;; (print (list :oin oindices))
	(dotimes (w (size window))
	  (let ((remaining w) (rmi 0) (valid t))
	    (loop :for cix :below wrank :for wf :across win-factors
	       :for oindex :in oindices :for if :across in-factors :for idim :across idims
	       :for melem :across movement :for wdim :across window-dims ;; :while valid
	       :do (multiple-value-bind (index remainder) (floor remaining wf)
		     (let ((this-index (+ index (- (* melem oindex)
						   (floor (- wdim (if (evenp wdim) 1 0)) 2)))))
		       (setf remaining remainder
			     (aref acoords cix) (if (> 0 this-index)
			     			    (max (aref acoords cix)
			     				 (abs this-index))
			     			    (if (<= idim this-index)
			     				(min (aref acoords cix)
			     				     (- (- this-index (1- idim))))
			     				(aref acoords cix))))
		       (if (<= 0 this-index (1- (aref idims cix)))
			   (setq rmi (+ rmi (* this-index if)))
			   (setq valid nil)))))
	    (setf (row-major-aref window w)
		  (if (not valid)
		      (apl-array-prototype input)
		      (row-major-aref input rmi)))))
	(setf (row-major-aref output o)
	      (funcall process window acoords))))
    output))

(defun count-segments (value precision &optional segments)
  "Count the lengths of segments a number will be divided into when printed using (array-impress), within the context of a column's existing segments if provided."
  (flet ((process-rational (number)
	   (list (write-to-string (numerator number))
		 (write-to-string (denominator number)))))
    (let* ((strings (if (typep value 'ratio)
			(process-rational value)
			(append (if (typep (realpart value) 'ratio)
				    (process-rational (realpart value))
				    (let* ((number-string (first (cl-ppcre:split
								  #\D (string-upcase
								       (write-to-string (realpart value))))))
					   (sections (cl-ppcre:split #\. number-string)))
				      ;; if there are 4 or more segments, as when printing complex floats or rationals,
				      ;; and a complex value occurs with an integer real part, create a 0-length
				      ;; second segment so that the lengths of the imaginary components are correctly
				      ;; assigned to the 3rd and 4th columns, as for printing ⍪12.2J44 3J8 19J210r17
				      (append sections (if (and (< 3 (length segments))
								(= 1 (length sections)))
							   (list nil)))))
				(if (complexp value)
				    (if (typep (imagpart value) 'ratio)
					(process-rational (imagpart value))
					(let ((number-string (first (cl-ppcre:split
								     #\D (string-upcase
									  (write-to-string (imagpart value)))))))
				          (cl-ppcre:split #\. number-string)))))))
	   (more-strings (< (length segments) (length strings))))
      ;; TODO: provide for e-notation
      (loop :for i :from 0 :for s :in (if more-strings strings segments)
	 :collect (if (> 0 precision)
		      (if (/= 0 (mod i 2))
			  (abs precision) (max (or (nth i segments) 0)
					       (length (nth i strings))))
		      (max (min (if (= 0 (mod i 2))
				    precision (- precision (- (max (or (nth (1- i) segments) 0)
								   (length (nth (1- i) strings)))
							      (if (and (nth (1- i) strings)
							   	       (char= #\- (aref (nth (1- i) strings)
											0)))
								  1 0))))
				(max (or (nth i segments) 0)
				     (length (nth i strings))))
			   (or (nth i segments) 0)))))))

(defun array-impress (input &key (prepend) (append) (collate) (in-collated) (format) (segment))
  "Render the contents of an array into a character matrix or, if the collate option is taken, an array with sub-matrices of characters."
  (cond ((or (functionp input) (= 0 (size input)))
	 (concatenate 'string (list #\Newline)))
	;; a function input produces an empty string, as does an empty array
	((not (arrayp input))
	 (if (characterp input) (string input)
	     (funcall format input (funcall segment input))))
	;; if indenting with a character, prepend it to the string; strings are otherwise passed back as-is
	((stringp input) input)
	;; each layer of 0-rank enclosure adds 1 space of indentation
	((= 0 (rank input))
	 (array-impress (aref input) :format format :segment segment :append append
			:in-collated collate :prepend (if (eq prepend t)
							  t (1+ (or prepend 0)))))
	(t (let* ((idims (dims input))
		  ;; the x-offset and y-offset for each column and row; each array has an extra element to
		  ;; represent the total width and height of the output array
		  (x-offsets (make-array (1+ (first (last idims)))
					 :initial-element 0 :element-type 'fixnum))
		  (y-offsets (make-array (1+ (reduce #'* (rest (reverse idims))))
					 :initial-element 0 :element-type 'fixnum))
		  (col-widths (make-array (first (last idims))
					  :initial-element 1 :element-type 'fixnum))
		  (col-types (make-array (first (last idims)) :initial-element nil))
		  (col-segments (make-array (first (last idims)) :initial-element nil))
		  (strings (make-array idims))
		  (output-default-char #\ )
		  (row) (empty-rows))
	     (declare (dynamic-extent output-default-char row empty-rows))
	     (symbol-macrolet ((this-string (apply #'aref strings coords))
			       (this-col-width (aref col-widths last-coord))
			       (this-col-type (aref col-types last-coord))
			       (last-col-type (aref col-types (1- last-coord)))
			       (segments (aref col-segments last-coord)))
	       (across input (lambda (elem coords)
			       (let* ((last-coord (first (last coords)))
				      (elem (disclose elem)))
				 (flet ((add-column-types (&rest types)
					  (loop :for type :in types
					     :do (if (not (member type this-col-type))
						     (setf this-col-type (cons type this-col-type))))))
				   (cond ((or (characterp elem)
					      (and (stringp elem) (= 1 (length elem))))
					  ;; characters are simply passed through,
					  ;; 1-item strings are rendered the same way
					  (add-column-types :character)
					  (setf this-string (if (characterp elem)
								elem (aref elem 0))))
					 ((arrayp elem)
					  ;; recurse to handle nested arrays, passing back the rendered character
					  ;; array and adjusting the offsets to allow for its height and width
					  (let ((rendered (array-impress elem :format format :segment segment
									 :prepend t)))
					    ;; in the case a 1D array (string) is passed back, height defaults to 1
					    (setf this-string rendered)
					    ;; TODO: improve element type-checking here
					    (if (eq 'character (element-type elem))
						(add-column-types :array :character-array)
						(if (or (eq 'fixnum (element-type elem))
							(eq 'bit (element-type elem)))
						    (add-column-types :array :number-array)
						    (add-column-types :array :mixed-array)))))
					 ((numberp elem)
					  ;; use the number-formatting function to make strings out of numbers,
					  ;; and adjust the next x-offset to allow for their width
					  (setf segments (funcall segment elem segments))
					  ;; numbers are not rendered until the next pass due
					  ;; to the need to measure segments for the entire column
					  (add-column-types :number)
					  (if (floatp elem)
					      (add-column-types :float)
					      (if (typep elem 'ratio)
						  (add-column-types :rational)))
					  ;; add flags for complex numbers and the types of numbers in the first
					  ;; column, needed for correct printing
					  (if (complexp elem)
					      (progn (add-column-types :complex)
						     (if (floatp (realpart elem))
							 (add-column-types :realpart-float)
							 (if (typep elem 'ratio)
							     (add-column-types :realpart-rational)))))))))))
	       (across input (lambda (elem coords)
			       (let* ((last-coord (first (last coords)))
				      (elem-width 1)
				      (elem-height 1)
				      (rendered (apply #'aref strings coords)))
				 (flet ((is-pure-character-column (&optional index)
					  (and (eq :character (first (aref col-types (or index last-coord))))
					       (not (rest (aref col-types (or index last-coord)))))))
				   (cond ((and (arrayp elem)
					       (not (characterp rendered)))
					  (let ((rdims (reverse (dims rendered))))
					    (setf elem-height (or (second rdims) 1)
						  elem-width (first rdims))))
					 ((numberp elem)
					  ;; pass the information on realpart type for correct printing
					  (let* ((elem-string (funcall format elem segments
								       (and (member :complex this-col-type)
									    (or (member :realpart-float
											this-col-type)
										(member :realpart-rational
											this-col-type))))))
					    (setf this-string elem-string
						  rendered (apply #'aref strings coords)
						  elem-width (length elem-string)))))
				   ;; if this is the beginning of a new row, increment the row's y-offset
				   ;; by the number of preceding empty rows
				   (if (= 0 last-coord)
				       (setf row (reduce
						  #'+ (mapcar #'* (rest (reverse coords))
							      (let ((current 1))
								(loop :for dim
								   :in (cons 1 (rest (reverse (rest idims))))
								   :collect (setq current
										  (* current dim))))))
					     ;; find the total number of empty lines preceding this row by
					     ;; encoding the coordinates excepting the last two with a series
					     ;; of number bases found by multiplying each dimension going
					     ;; backwards excepting the last 2 by the previous base and adding 1
					     empty-rows
					     (reduce
					      #'+ (mapcar #'* (cddr (reverse coords))
							  (cons 1 (let ((last 1))
								    (loop :for dim
								       :in (reverse (rest (butlast idims 2)))
								       :collect (setq last
										      (1+ (* dim last))))))))
					     (aref y-offsets row)
					     (+ empty-rows (aref y-offsets row))))
				   (setf this-col-width (max this-col-width elem-width)
					 (aref y-offsets (1+ row))
					 (max (aref y-offsets (1+ row))
					      (+ elem-height (aref y-offsets row) (- empty-rows))
					      (if (and (= 2 (length y-offsets))
						       (< 1 (aref y-offsets (1+ row))))
						  ;; don't increment the next row if it is the last, there's only
						  ;; one row in the output and its value is higher than one;
						  ;; this mainly applies to vectors containing nested arrays
						  ;; of rank>1
						  (aref y-offsets (1+ row))
						  (if (= row (- (length y-offsets) 2))
						      (+ elem-height (aref y-offsets row))
						      (+ row (if (and (= 0 last-coord)
								      (/= last-coord (1- (length y-offsets))))
								 1 0))))))))))
	       ;; (print (list :xoyo x-offsets y-offsets col-widths col-decimals))
	       ;; collated output is printed to a multidimensional array whose sub-matrices are character-rendered
	       ;; versions of the original sub-matrices, as per APL's [⍕ format] function. If a prepend
	       ;; character is set, the output array has an extra element in its last dimension to hold the
	       ;; indenting character
	       ;; (print (list :seg col-segments col-widths x-offsets))
	       (let ((total 0))
		 (loop :for s :below (length col-segments) :for i :from 0
		    :do (let ((char-column (and (eq :character (first (aref col-types s)))
						(not (rest (aref col-types s))))))
			  (setf (aref col-widths s) (if (aref col-segments s)
							;; if there are segment lengths for this column, its
							;; width is the greater of the total segment length
							;; plus spacing characters, or the calculated column
							;; width derived from its array contents
							(max (aref col-widths s)
							     (+ (1- (length (aref col-segments s)))
								(reduce #'+ (aref col-segments s))))
							(aref col-widths s))
				total (+ total (if (and prepend (= 0 i) (not (eq t prepend))) prepend 0)
					 ;; add a prepending space if this is an array column
					 ;; or if the prior column held arrays, this column doesn't
					 ;; and this column is not a character-only column
					 (if (or (and (member :array (aref col-types s))
						      (not (and (/= 0 s)
								(eq :character (first (aref col-types (1- s))))
								(not (rest  (aref col-types (1- s)))))))
						 (and (/= 0 s) (not char-column)
						      (member :array (aref col-types (1- s)))))
					     1 0))
				(aref x-offsets s) total
				;; set the x-offset at the current index to the calculated total up to this point,
				;; then increment the total x-offset to the lowest possible value for the
				;; next x-offset, first by adding the current column's width
				total (+ total (aref col-widths s)
					 ;; add another space if this isn't the last column and it isn't
					 ;; a character-only column
					 (if (and (/= s (1- (length col-segments)))
						  (or (not char-column)
						      (not (and (eq :character (first (aref col-types (1+ s))))
								(not (rest (aref col-types (1+ s))))))))
					     1 0)))))
		 (setf (aref x-offsets (1- (length x-offsets)))
		       (- total (if (not prepend)
				    0 (if (eq t prepend) 1 prepend)))))
	       
	       (let* ((to-collate (and collate (or (not (eq t (element-type input)))
						   (loop :for i :below (array-total-size input)
						      :always (not (arrayp (row-major-aref input i)))))))
		      (output (if to-collate
				  (make-array (append (butlast idims)
						      (list (+ (if collate 0 1)
							       (aref x-offsets (1- (length x-offsets))))))
					      :element-type 'character :initial-element output-default-char)
				  (make-array (list (aref y-offsets (1- (length y-offsets)))
						    (+ (if (or collate in-collated) 0 1)
						       (if (and prepend (not (eq t prepend))) prepend 0)
						       (aref x-offsets (1- (length x-offsets)))))
					      :element-type 'character :initial-element output-default-char))))
		 (across strings
			 (lambda (chars coords)
			   ;; calculate the row of output currently being produced
			   (let* ((row (reduce #'+ (mapcar #'* (rest (reverse coords))
							   (let ((current 1))
							     (loop :for dim
								:in (cons 1 (rest (reverse (rest idims))))
								:collect (setq current (* current dim)))))))
				  (last-coord (first (last coords)))
				  (chars-width (first (last (dims chars)))))
			     (flet ((is-pure-character-column (&optional index)
				      (and (eq :character (first (aref col-types (or index last-coord))))
					   (not (rest (aref col-types (or index last-coord))))))
				    (is-character-array-column (&optional index)
				      (let ((type-list (aref col-types (or index last-coord))))
					(and (not (member :number type-list))
					     (not (member :mixed-array type-list))
					     (not (member :number-array type-list))))))
			       (if (arrayp chars)
				   ;; print a string or sub-matrix of characters; the coordinate conversion
				   ;; is different depending on whether collated output is being produced
				   (across chars
					   (lambda (element ecoords)
					     (let* ((x-coord (+ (or (second ecoords) (first ecoords))
								(if (is-pure-character-column last-coord)
								    0 (aref x-offsets last-coord))
								;; if this is not a pure numeric column,
								;; right-align the values
								(if (or (not (member :number this-col-type))
									(not (member :array this-col-type)))
								    0 (- this-col-width chars-width)))))
					       (if to-collate
						   (setf (apply #'aref output (append (butlast coords 1)
										      (list x-coord)))
								 element)
						   (setf (aref output (+ (aref y-offsets row)
									 (if (not (second ecoords))
									     0 (first ecoords)))
							       x-coord)
							 element)))))
				   ;; print a single character
				   (let ((x-coord (+ (aref x-offsets last-coord)
						     (if (not (member :number this-col-type))
							 0 (1- this-col-width)))))
				     (if to-collate (setf (apply #'aref output (append (butlast coords 1)
										       (list x-coord)))
							  chars)
					 (setf (aref output (aref y-offsets row) x-coord)
					       chars))))))))
		 ;; if prepending or appending a character, it is placed in the array here;
		 ;; this is more complicated for a collated array and it is not needed if the
		 ;; character is the same as the default character for the array
		 ;; (princ #\newline)
		 (if (and append (not (char= append output-default-char)))
		     (let ((last-dim (first (last (dims output)))))
		       (if to-collate (across output (lambda (elem coords)
						       (declare (ignore elem))
						       (setf (apply #'aref output coords) append)))
			   (if append (dotimes (row (first (dims output)))
					 (setf (aref output row (1- last-dim)) append))))))
		 output))))))

(defmacro matrix-print (input &rest options)
  "Print a character matrix generated by array-impress."
  (let ((rendered (gensym)))
    `(let ((,rendered (array-impress ,input ,@options)))
       (if (stringp ,rendered)
	   ,rendered (make-array (array-total-size ,rendered)
				 :element-type 'character :displaced-to ,rendered)))))

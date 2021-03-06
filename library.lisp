;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; library.lisp

(in-package #:april)

"This file contains the functions in April's 'standard library' that aren't provided by the aplesque package, mostly functions that are specific to the APL language and not generally applicable to array processing."

(defun deal (index-origin)
  "Return a function to randomly shuffle a finite sequence. Used to implement [? deal]."
  (lambda (omega alpha)
    (let ((omega (disclose-unitary omega))
	  (alpha (disclose-unitary alpha)))
      (if (or (not (integerp omega))
	      (not (integerp alpha)))
	  (error "Both arguments to ? must be single non-negative integers.")
	  (if (> alpha omega)
	      (error "The left argument to ? must be less than or equal to the right argument.")
	      (let ((vector (count-to omega index-origin)))
		;; perform Knuth shuffle of vector
		(loop :for i :from omega :downto 2 :do (rotatef (aref vector (random i))
								(aref vector (1- i))))
		(if (= alpha omega)
		    vector (make-array alpha :displaced-to vector :element-type (element-type vector)))))))))

(defun apl-divide (method)
  "Generate a division function according to the [⎕DIV division method] in use."
  (lambda (omega &optional alpha)
    (if (and alpha (= 0 omega) (= 0 alpha))
	(if (= 0 method) 1 0)
	(if alpha (/ alpha omega)
	    (if (and (< 0 method) (= 0 omega))
		0 (/ omega))))))

(defun complex-floor (number)
  "Find the floor of a complex number using Eugene McDonnell's algorithm."
  (let* ((r (realpart number))
	 (i (imagpart number))
	 (b (+ (floor r) (* #C(0 1) (floor i))))
	 (x (mod r 1))
	 (y (mod i 1)))
    (+ b (if (> 1 (+ x y))
	     0 (if (>= x y) 1 #C(0 1))))))

(defun apl-floor (omega)
  "Find a number's floor using the complex floor algorithm if needed."
  (if (complexp omega) (complex-floor omega)
      (floor omega)))

(defun apl-ceiling (omega)
  "Find a number's ceiling deriving from the complex floor algorithm if needed."
  (if (complexp omega) (- (complex-floor (- omega)))
      (ceiling omega)))

(defun apl-residue (omega alpha)
  "Implementation of residue extended to complex numbers based on the complex-floor function"
  (if (or (complexp omega) (complexp alpha))
      (let ((ainput (complex (if (= 0 (realpart alpha))
                                 1 (realpart alpha))
                             (if (= 0 (imagpart alpha))
                                 1 (imagpart alpha)))))
        (- omega (* alpha (complex-floor (/ omega alpha)))))
      (mod omega alpha)))

(defun apl-gcd (omega alpha)
  "Implementation of greatest common denominator extended to complex numbers based on the complex-floor function."
  (if (or (complexp omega) (complexp alpha))
      (if (= 0 (apl-residue omega alpha))
          alpha (apl-gcd alpha (apl-residue omega alpha)))
      (funcall (apl-xcy #'gcd) omega alpha)))

(defun apl-lcm (omega alpha)
  "Implementation of lease common multiple extended to complex numbers based on the complex-floor function."
  (if (or (complexp omega) (complexp alpha))
      (* alpha (/ omega (apl-gcd omega alpha)))
      (funcall (apl-xcy #'lcm) omega alpha)))

(defun without (omega alpha)
  "Remove elements in omega from alpha. Used to implement dyadic [~ without]."
  (flet ((compare (o a)
	   (funcall (if (and (characterp a) (characterp o))
			#'char= (if (and (numberp a) (numberp o))
				    #'= (lambda (a o) (declare (ignore a o)))))
		    o a)))
    (if (not (arrayp alpha))
	(setq alpha (vector alpha))
	(if (not (vectorp alpha))
	    (error "The left argument to [~ without] must be a vector.")))
    (let ((included)
	  (omega-vector (if (or (vectorp omega)	(not (arrayp omega)))
			    (disclose omega)
			    (make-array (array-total-size omega)
					:displaced-to omega :element-type (element-type omega)))))
      (loop :for element :across alpha
	 :do (let ((include t))
	       (if (vectorp omega-vector)
		   (loop :for ex :across omega-vector
		      :do (if (compare ex element) (setq include nil)))
		   (if (compare omega-vector element) (setq include nil)))
	       (if include (setq included (cons element included)))))
      (make-array (list (length included)) :element-type (element-type alpha)
		  :initial-contents (reverse included)))))

(defun apl-xcy (function)
  "Return a function to find the greatest common denominator or least common multiple of fractional as well as whole numbers. If one or both arguments are floats, the result is coerced to a double float."
  (lambda (omega alpha)
    (if (and (integerp omega) (integerp alpha))
	(funcall function omega alpha)
	(let* ((float-input)
	       (omega (if (not (floatp omega))
			  omega (setf float-input (rationalize omega))))
	       (alpha (if (not (floatp alpha))
			  alpha (setf float-input (rationalize alpha)))))
	  (funcall (if (not float-input) #'identity (lambda (number)
						      (if (not (typep number 'ratio))
							  number (coerce number 'double-float))))
		   (let ((d-product (* (denominator omega) (denominator alpha))))
		     (/ (funcall function (* d-product omega) (* d-product alpha))
			d-product)))))))

(defun scalar-compare (comparison-tolerance)
  "Compare two scalar values as appropriate for APL."
  (lambda (omega alpha)
    (funcall (if (and (characterp alpha) (characterp omega))
		 #'char= (if (and (numberp alpha) (numberp omega))
			     (if (not (or (floatp alpha) (floatp omega)))
				 #'= (lambda (a o) (> comparison-tolerance (abs (- a o)))))
			     (lambda (a o) (declare (ignore a o)))))
	     omega alpha)))

(defun compare-by (symbol comparison-tolerance)
  "Generate a comparison function using the [⎕CT comparison tolerance]."
  (lambda (omega alpha)
    (funcall (if (and (numberp alpha) (numberp omega))
		 (if (not (or (floatp alpha) (floatp omega)))
		     (symbol-function symbol) (lambda (a o) (and (< comparison-tolerance (abs (- a o)))
								 (funcall (symbol-function symbol) a o)))))
	     omega alpha)))

(defun count-to (index index-origin)
  "Implementation of APL's ⍳ function."
  (let ((index (disclose index)))
    (if (or (integerp index)
	    (and (vectorp index)
		 (= 1 (length index))))
	(let ((index (if (not (vectorp index)) index (row-major-aref index 0))))
	  (if (= 0 index) (vector)
	      (let ((output (make-array index :element-type (list 'integer 0 index))))
		(xdotimes output (i index) (setf (aref output i) (+ i index-origin)))
		output)))
	(if (vectorp index)
	    (let ((output (make-array (array-to-list index))))
	      (across output (lambda (elem coords)
			       (declare (ignore elem))
			       (setf (apply #'aref output coords)
				     (make-array (length index)
						 :element-type
						 (list 'integer 0 (+ index-origin (reduce #'max coords)))
						 :initial-contents
						 (if (= 0 index-origin)
						     coords (loop :for c :in coords
							       :collect (+ c index-origin)))))))
	      output)
	    (error "The argument to [⍳ index] must be an integer, i.e. ⍳9, or a vector, i.e. ⍳2 3.")))))

(defun inverse-count-to (vector index-origin)
  "The [⍳ index] function inverted; it returns the length of a sequential integer array starting from the index origin or else throws an error."
  (if (not (vectorp vector))
      (error "Inverse [⍳ index] can only be invoked on a vector, at least for now.")
      (if (loop :for e :across vector :for i :from index-origin :always (= e i))
	  (length vector) (error "The argument to inverse [⍳ index] is not an index vector."))))

(defun shape (omega)
  "Get the shape of an array, implementing monadic [⍴ shape]."
  (if (or (not (arrayp omega))
	  (= 0 (rank omega)))
      #() (if (and (listp (type-of omega))
		   (eql 'simple-array (first (type-of omega)))
		   (eq t (second (type-of omega)))
		   (eq nil (third (type-of omega))))
	      0 (if (vectorp omega)
		    (make-array 1 :element-type (list 'integer 0 (length omega))
				:initial-contents (list (length omega)))
		    (let* ((omega-dims (dims omega))
			   (max-dim (reduce #'max omega-dims)))
		      (make-array (length omega-dims)
				  :initial-contents omega-dims :element-type (list 'integer 0 max-dim)))))))

(defun reshape-array (metadata-symbol)
  "Wrap (aplesque:reshape-to-fit) so that dyadic [⍴ shape] can be implemented with the use of empty-array prototypes."
  (lambda (omega alpha)
    (let ((output (reshape-to-fit omega (if (arrayp alpha) (array-to-list alpha)
					    (list alpha))
				  :populator (build-populator metadata-symbol omega))))
      (if (and (= 0 (size output)) (arrayp omega)
	       (arrayp (row-major-aref omega 0)))
	  (set-workspace-item-meta metadata-symbol output
				   :eaprototype
				   (make-prototype-of (funcall (if (= 0 (rank omega)) #'identity #'aref)
							       (row-major-aref omega 0)))))
      output)))

(defun at-index (omega alpha axes index-origin &optional to-set)
  "Find the value(s) at the given index or indices in an array. Used to implement [⌷ index]."
  (if (not (arrayp omega))
      (if (and (numberp alpha)
	       (= index-origin alpha))
	  omega (error "Invalid index."))
      (multiple-value-bind (assignment-output assigned-array)
	  (choose omega (let ((coords (funcall (if (arrayp alpha) #'array-to-list #'list)
					       (apply-scalar #'- alpha index-origin)))
			      ;; the inefficient array-to-list is used here in case of nested
			      ;; alpha arguments like (⊂1 2 3)⌷...
			      (axis (if axes (if (vectorp (first axes))
						 (loop :for item :across (first axes)
						    :collect (- item index-origin))
						 (if (integerp (first axes))
						     (list (- (first axes) index-origin)))))))
			  (if (not axis)
			      ;; pad coordinates with nil elements in the case of an elided reference
			      (append coords (loop :for i :below (- (rank omega) (length coords)) :collect nil))
			      (loop :for dim :below (rank omega)
				 :collect (if (member dim axis) (first coords))
				 :when (member dim axis) :do (setq coords (rest coords)))))
		  :set to-set)
	(or assigned-array assignment-output))))

(defun find-depth (omega)
  "Find the depth of an array, wrapping (aplesque:array-depth). Used to implement [≡ depth]."
  (if (not (arrayp omega))
      0 (array-depth omega)))

(defun find-first-dimension (omega)
  "Find the first dimension of an array. Used to implement [≢ first dimension]."
  (if (= 0 (rank omega))
      1 (first (dims omega))))

(defun membership (omega alpha)
  "Determine if elements of alpha are present in omega. Used to implement dyadic [∊ membership]."
  (flet ((compare (item1 item2)
	   (if (and (characterp item1) (characterp item2))
	       (char= item1 item2)
	       (if (and (numberp item1) (numberp item2))
		   (= item1 item2)
		   (if (and (arrayp item1) (arrayp item2))
		       (array-compare item1 item2))))))
    (if (not (arrayp alpha))
	(if (not (arrayp omega))
	    (if (compare omega alpha) 1 0)
	    (if (not (loop :for item :across omega :never (compare item alpha)))
		1 0))
	(let* ((output (make-array (dims alpha) :element-type 'bit :initial-element 0))
	       (omega (enclose-atom omega))
	       (to-search (if (vectorp omega)
			      omega (make-array (array-total-size omega)
						:displaced-to omega :element-type (element-type omega)))))
	  ;; TODO: this could be faster with use of a hash table and other additions
	  (xdotimes output (index (array-total-size output))
	    (let ((found))
	      (loop :for item :across to-search :while (not found)
		 :do (setq found (compare item (row-major-aref alpha index))))
	      (if found (setf (row-major-aref output index) 1))))
	  output))))

(defun where-equal-to-one (omega index-origin)
  "Return a vector of coordinates from an array where the value is equal to one. Used to implement [⍸ where]."
  (let* ((indices) (match-count 0)
	 (orank (rank omega)))
    (if (= 0 orank)
	(if (= 1 omega) #(#()) #())
	(progn (across omega (lambda (index coords)
			       ;; (declare (dynamic-extent index coords))
			       (if (= 1 index)
				   (let* ((max-coord 0)
					  (coords (mapcar (lambda (i)
							    (setq max-coord
								  (max max-coord (+ i index-origin)))
							    (+ i index-origin))
							  coords)))
				     (incf match-count)
				     (setq indices (cons (if (< 1 orank)
							     (make-array
							      orank :element-type (list 'integer 0 max-coord)
							      :initial-contents coords)
							     (first coords))
							 indices))))))
	       (if (not indices)
		   #() (make-array match-count :element-type (if (< 1 orank)
								 t (list 'integer 0 (reduce #'max indices)))
				   :initial-contents (reverse indices)))))))

(defun tabulate (omega)
  "Return a two-dimensional array of values from an array, promoting or demoting the array if it is of a rank other than two. Used to implement [⍪ table]."
  (if (not (arrayp omega))
      omega (if (vectorp omega)
		(let ((output (make-array (list (length omega) 1) :element-type (element-type omega))))
		  (loop :for i :below (length omega) :do (setf (row-major-aref output i) (aref omega i)))
		  output)
		(let ((o-dims (dims omega)))
		  (make-array (list (first o-dims) (reduce #'* (rest o-dims)))
			      :element-type (element-type omega)
			      :displaced-to (copy-nested-array omega))))))

(defun ravel-array (index-origin)
  "Wrapper for aplesque [, ravel] function incorporating index origin from current workspace."
  (lambda (omega &optional axes)
    (ravel index-origin omega axes)))

(defun catenate-arrays (index-origin)
  "Wrapper for [, catenate] incorporating (aplesque:catenate) and (aplesque:laminate)."
  (lambda (omega alpha &optional axes)
    (let ((axis *first-axis-or-nil*))
      (if (and (floatp axis)
	       (< double-float-epsilon (nth-value 1 (floor axis))))
	  ;; laminate in the case of a fractional axis argument
	  (laminate alpha omega (ceiling axis))
	  ;; simply stack the arrays if there is no axis argument or it's an integer
	  (catenate alpha omega (or (if axis (floor axis))
				    (max 0 (1- (max (rank alpha) (rank omega))))))))))

(defun catenate-on-first (index-origin)
  "Wrapper for [⍪ catenate first]; distinct from (catenate-arrays) because it does not provide the laminate functionality."
  (lambda (omega alpha &optional axes)
    (if (and (vectorp alpha) (vectorp omega))
	(if (and *first-axis-or-nil* (< 0 *first-axis-or-nil*))
	    (error (concatenate 'string "Specified axis is greater than 1, vectors"
				" have only one axis along which to catenate."))
	    (if (and axes (> 0 *first-axis-or-nil*))
		(error (format nil "Specified axis is less than ~a." index-origin))
		(catenate alpha omega 0)))
	(if (or (not axes)
		(integerp (first axes)))
	    (catenate alpha omega (or *first-axis-or-nil* 0))))))

(defun section-array (index-origin metadata-symbol &optional inverse)
  "Wrapper for (aplesque:section) used for [↑ take] and [↓ drop]."
  (lambda (omega alpha &optional axes)
    (let* ((alpha-index alpha)
	   (alpha (if (arrayp alpha)
		      alpha (vector alpha)))
	   (output (section omega
			    (if axes (let ((dims (make-array
						  (rank omega)
						  :initial-contents (if inverse (loop :for i :below (rank omega)
										   :collect 0)
									(dims omega))))
					   (spec-axes (first axes)))
				       (if (integerp spec-axes)
					   (setf (aref dims (- spec-axes index-origin)) (aref alpha 0))
					   (if (vectorp spec-axes)
					       (loop :for ax :across spec-axes :for ix :from 0
						  :do (setf (aref dims (- ax index-origin))
							    (aref alpha ix)))))
				       dims)
				alpha)
			    :inverse inverse :populator (build-populator metadata-symbol omega))))
      ;; if the resulting array is empty and the original array prototype was an array, set the
      ;; empty array prototype accordingly
      (if (and (= 0 (size output)) (not inverse)
	       (arrayp omega) (arrayp (row-major-aref omega 0)))
	  (set-workspace-item-meta metadata-symbol output
				   :eaprototype
				   (make-prototype-of (funcall (if (= 0 (rank omega)) #'identity #'aref)
							       (row-major-aref omega 0)))))
      output)))

(defun pick (index-origin)
  "Fetch an array element, within successively nested arrays for each element of the left argument."
  (lambda (omega alpha)
    (labels ((pick-point (point input)
	       (if (is-unitary point)
		   (let ((point (disclose point)))
		     ;; if this is the last level of nesting specified, fetch the element
		     (if (not (arrayp point))
			 (aref input (- point index-origin))
			 (if (vectorp point)
			     (apply #'aref input (loop :for p :across point :collect (- p index-origin)))
			     (error "Coordinates for ⊃ must be expressed by scalars or vectors."))))
		   ;; if there are more elements of the left argument left to go, recurse on the element designated
		   ;; by the first element of the left argument and the remaining elements of the point
		   (pick-point (if (< 2 (length point))
				   (make-array (1- (length point))
					       :initial-contents (loop :for i :from 1 :to (1- (length point))
								    :collect (aref point i)))
				   (aref point 1))
			       (disclose (pick-point (aref point 0) input))))))
      ;; TODO: swap out the vector-based point for an array-based point
      (pick-point alpha omega))))

(defun expand-array (degrees input axis metadata-symbol &key (compress-mode))
  "Wrapper for (aplesque:expand) implementing [/ replicate] and [\ expand]."
  (let ((output (expand degrees input axis :compress-mode compress-mode
			:populator (build-populator metadata-symbol input))))
    (if (and (= 0 (size output)) (arrayp input) (not (= 0 (size input)))
             (arrayp (row-major-aref input 0)))
	(set-workspace-item-meta metadata-symbol output
				 :eaprototype
				 (make-prototype-of (funcall (if (= 0 (rank input)) #'identity #'aref)
							     (row-major-aref input 0)))))
    output))

(defun array-intersection (omega alpha)
  "Return a vector of values common to two arrays. Used to implement [∩ intersection]."
  (let ((omega (enclose-atom omega))
	(alpha (enclose-atom alpha)))
    (if (or (not (vectorp alpha))
	    (not (vectorp omega)))
	(error "Arguments to [∩ intersection] must be vectors.")
	(let* ((match-count 0)
	       (matches (loop :for item :across alpha :when (find item omega :test #'array-compare)
			   :collect item :and :do (incf match-count))))
	  (make-array (list match-count) :initial-contents matches
		      :element-type (type-in-common (element-type alpha) (element-type omega)))))))

(defun unique (omega)
  "Return a vector of unique values in an array. Used to implement [∪ unique]."
  (if (not (arrayp omega))
      (vector omega)
      (if (= 0 (rank omega))
	  (vector omega)
	  (let ((vector (if (vectorp omega)
			    omega (re-enclose omega (make-array (max 0 (1- (rank omega)))
								:element-type 'fixnum
								:initial-contents
								(loop :for i :from 1 :to (1- (rank omega))
								   :collect i))))))
	    (let ((uniques) (unique-count 0))
	      (loop :for item :across vector :when (not (find item uniques :test #'array-compare))
		 :do (setq uniques (cons item uniques)
			   unique-count (1+ unique-count)))
	      (funcall (lambda (result) (if (vectorp omega) result (mix-arrays 1 result)))
		       (make-array unique-count :element-type (element-type vector)
				   :initial-contents (reverse uniques))))))))

(defun array-union (omega alpha)
  "Return a vector of unique values from two arrays. Used to implement [∪ union]."
  (let ((omega (enclose-atom omega))
	(alpha (enclose-atom alpha)))
    (if (or (not (vectorp alpha))
	    (not (vectorp omega)))
	(error "Arguments must be vectors.")
	(let* ((unique-count 0)
	       (uniques (loop :for item :across omega :when (not (find item alpha :test #'array-compare))
			   :collect item :and :do (incf unique-count))))
	  (catenate alpha (make-array unique-count :initial-contents uniques
				      :element-type (type-in-common (element-type alpha)
								    (element-type omega)))
		    0)))))

(defun unique-mask (array)
  "Return a 1 for each value encountered the first time in an array, 0 for others. Used to implement monadic [≠ unique mask]."
  (let ((output (make-array (first (dims array)) :element-type 'bit :initial-element 1))
	(displaced (if (< 1 (rank array)) (make-array (rest (dims array))
						      :displaced-to array
						      :element-type (element-type array))))
	(uniques) (increment (reduce #'* (rest (dims array)))))
    (dotimes (x (first (dims array)))
      (if (and displaced (< 0 x))
	  (setq displaced (make-array (rest (dims array)) :element-type (element-type array)
				      :displaced-to array :displaced-index-offset (* x increment))))
      (if (member (or displaced (aref array x)) uniques :test #'array-compare)
	  (setf (aref output x) 0)
	  (setf uniques (cons (if displaced (make-array (rest (dims array)) :displaced-to array
							:element-type (element-type array)
							:displaced-index-offset (* x increment))
				  (aref array x))
			      uniques))))
    output))

(defun permute-array (index-origin)
  "Wraps (aops:permute) to permute an array, rearranging the axes in a given order or reversing them if no order is given. Used to implement monadic and dyadic [⍉ permute]."
  (lambda (omega &optional alpha)
    (if (not (arrayp omega))
	omega (progn (if alpha (if (not (arrayp alpha))
				   (setq alpha (- alpha index-origin))
				   (if (vectorp alpha)
				       (if (> (length alpha) (rank omega))
					   (error "Length of left argument to ⍉ must be equal to rank of right argument.")
					   (loop :for a :across alpha :for ax :from 0
					      :do (setf (aref alpha ax) (- a index-origin))))
				       (error "Left argument to ⍉ must be a scalar or vector."))))
		     (permute-axes omega alpha)))))

(defun matrix-inverse (omega)
  "Invert a matrix. Used to implement monadic [⌹ matrix inverse]."
  (if (not (arrayp omega))
      (/ omega)
      (if (< 2 (rank omega))
	  (error "Matrix inversion only works on arrays of rank 2 or 1.")
	  (funcall (if (and (= 2 (rank omega)) (reduce #'= (dims omega)))
		       #'invert-matrix #'left-invert-matrix)
		   omega))))

(defun matrix-divide (omega alpha)
  "Divide two matrices. Used to implement dyadic [⌹ matrix divide]."
  (array-inner-product (invert-matrix omega) alpha (lambda (arg1 arg2) (apply-scalar #'* arg1 arg2))
		       #'+))

(defun encode (omega alpha &optional inverse)
  "Encode a number or array of numbers as per a given set of bases. Used to implement [⊤ encode]."
  (if (and (vectorp alpha) (= 0 (length alpha)))
      #() (let* ((omega (if (arrayp omega)
			    omega omega))
		 (alpha (if (arrayp alpha)
			    alpha (if (not inverse)
				      ;; if the encode is an inverted decode, extend a
				      ;; scalar left argument to the appropriate degree
				      alpha
				      (let ((max-omega 0))
					(if (arrayp omega)
					    (dotimes (i (size omega))
					      (setq max-omega (max max-omega (row-major-aref omega i))))
					    (setq max-omega omega))
					(make-array (1+ (floor (log max-omega) (log alpha)))
						    :initial-element alpha)))))
		 (odims (dims omega)) (adims (dims alpha))
		 (osize (size omega)) (asize (size alpha))
		 (last-adim (first (last adims)))
		 (out-coords (loop :for i :below (+ (- (rank alpha) (count 1 adims))
						    (- (rank omega) (count 1 odims))) :collect 0))
		 (out-dims (append (loop :for dim :in adims :when t :collect dim)
				   (loop :for dim :in odims :when t :collect dim)))
		 ;; currently, the output is set to t because due to the cost of finding the highest array value
		 (output (if out-dims (make-array out-dims)))
		 (aseg-last (reduce #'* (butlast adims 1)))
		 (aseg-first (reduce #'* (rest adims)))
		 (ofactor (* osize aseg-first)))
	    (dotimes (i (size output))
	      (multiple-value-bind (o a) (floor i asize)
      		(let ((value (if (not (arrayp omega))
				 omega (row-major-aref omega o)))
      		      (last-base 1) (base 1) (component 1) (element 0)
		      (increment (if (/= 1 aseg-last) aseg-last asize)))
      		  (loop :for index :from (1- increment) :downto (mod a increment)
      		     :do (setq last-base base
      			       base (* base (if (not (arrayp alpha))
						alpha (row-major-aref alpha (+ (* index aseg-first)
									       (floor a increment)))))
      			       component (if (= 0 base) value (nth-value 1 (floor value base)))
      			       value (- value component)
      			       element (if (= 0 last-base) 0 (floor component last-base))))
      		  (if output (setf (row-major-aref output (+ o (* osize (floor a aseg-last))
							     (* ofactor (mod a aseg-last))))
      				   element)
		      (setq output element)))))
	    output)))

(defun decode (omega alpha)
  "Decode an array of numbers as per a given set of bases. Used to implement [⊥ decode]."
  (let* ((omega (if (arrayp omega) omega (enclose-atom omega)))
	 (alpha (if (arrayp alpha) alpha (enclose-atom alpha)))
	 (odims (dims omega)) (adims (dims alpha))
	 (osize (size omega)) (asize (size alpha))
	 (out-coords (loop :for i :below (max 1 (+ (1- (rank alpha)) (1- (rank omega)))) :collect 0))
	 (out-dims (append (butlast adims 1) (rest odims)))
	 (output (if out-dims (make-array out-dims)))
	 (ovector (first odims))
	 ;; extend a 1-column matrix to the length of the right argument's first dimension,
	 ;; supporting use cases like (⍪5 10)⊥4 8⍴⍳9
	 (alpha (if (< 1 asize) (if (not (and (< 1 osize) (= 1 (first (last adims)))))
				    alpha (let ((out (make-array (append (butlast adims 1)
									 (list ovector)))))
					    (dotimes (a asize)
					      (dotimes (i ovector)
						(setf (row-major-aref out (+ i (* a ovector)))
						      (row-major-aref alpha a))))
					    out))
		    (make-array ovector :initial-element (row-major-aref alpha 0))))
	 (olvector (first (last odims)))
	 (afactors (make-array (if (and (< 1 osize) (< 1 (first (last adims))))
				   adims (append (butlast adims 1) (list ovector)))
			       :initial-element 1))
	 (asegments (reduce #'* (butlast adims 1)))
	 (av2 (first (last (dims alpha))))
	 (out-section (reduce #'* (rest odims))))
    (if out-dims (progn (dotimes (a asegments)
			  (loop :for i :from (- (* av2 (1+ a)) 2) :downto (* av2 a)
			     :do (setf (row-major-aref afactors i) (* (row-major-aref alpha (1+ i))
								      (row-major-aref afactors (1+ i))))))
			(xdotimes output (i (size output))
    			  (multiple-value-bind (a o) (floor i olvector)
      			    (let ((result 0))
    			      (loop :for index :below av2
    	      			 :do (incf result (* (row-major-aref omega (mod (+ (mod i out-section)
										   (* out-section index))
										(size omega)))
    	      					     (row-major-aref afactors
								     (+ index (* av2 (floor i out-section)))))))
    			      (setf (row-major-aref output i) result)))))
    	(let ((result 0) (factor 1))
    	  (loop :for i :from (1- (if (< 1 av2) av2 ovector)) :downto 0
    	     :do (incf result (* factor (row-major-aref omega (min i (1- ovector)))))
    	       (setq factor (* factor (row-major-aref alpha (min i (1- av2))))))
	  (setq output result)))
    output))

(defun left-invert-matrix (in-matrix)
  "Perform left inversion of matrix. Used to implement [⌹ matrix inverse]."
  (let* ((input (if (= 2 (rank in-matrix))
		    in-matrix (make-array (list (length in-matrix) 1))))
	 (input-displaced (if (/= 2 (rank in-matrix))
			      (make-array (list 1 (length in-matrix)) :element-type (element-type input)
					  :displaced-to input))))
    (if input-displaced (xdotimes input (i (length in-matrix)) (setf (row-major-aref input i)
								     (aref in-matrix i))))
    (let ((result (array-inner-product (invert-matrix (array-inner-product (or input-displaced
									       (aops:permute '(1 0) input))
									   input #'* #'+))
				       (or input-displaced (aops:permute '(1 0) input))
				       #'* #'+)))
      (if (= 1 (rank in-matrix))
	  (make-array (size result) :element-type (element-type result) :displaced-to result)
	  result))))

(defun format-array (print-precision)
  "Use (aplesque:array-impress) to print an array and return the resulting character array, with the option of specifying decimal precision. Used to implement monadic and dyadic [⍕ format]."
  (lambda (omega &optional alpha)
    (if (and alpha (not (integerp alpha)))
	(error (concatenate 'string "The left argument to ⍕ must be an integer specifying"
			    " the precision at which to print floating-point numbers.")))
    (array-impress omega :collate t
		   :segment (lambda (number &optional segments)
			      (count-segments number (if alpha (- alpha) print-precision)
					      segments))
		   :format (lambda (number &optional segments rps)
			     (print-apl-number-string number segments print-precision alpha rps)))))

(defun format-array-uncollated (print-precision-default)
  "Generate a function using (aplesque:array-impress) to print an array in matrix form without collation. Used to implement ⎕FMT."
  (lambda (input &optional print-precision)
    (let ((print-precision (or print-precision print-precision-default))
          (is-not-nested t))
      (if (and print-precision (not (integerp print-precision)))
          (error (concatenate 'string "The left argument to ⍕ must be an integer specifying"
			      " the precision at which to print floating-point numbers.")))
      ;; only right-indent if this is a nested array; this is important for box-drawing functions
      (if (arrayp input) (xdotimes input (x (size input))
                           (if (arrayp (row-major-aref input x))
                               (setf is-not-nested nil))))
      (funcall (lambda (output)
                 (if (/= 1 (rank output))
                     output (array-promote output)))
               (array-impress input :unpadded is-not-nested
                              :segment (lambda (number &optional segments)
			                 (count-segments number print-precision segments))
		              :format (lambda (number &optional segments rps)
			                (print-apl-number-string number segments
                                                                 print-precision print-precision rps)))))))

(defun generate-index-array (array)
  "Given an array, generate an array of the same shape whose each cell contains its row-major index."
  (let* ((is-scalar (= 0 (rank array)))
	 (array (if is-scalar (aref array) array))
	 (output (make-array (dims array) :element-type (list 'integer 0 (size array)))))
    (xdotimes output (i (size array)) (setf (row-major-aref output i) i))
    (funcall (if (not is-scalar) #'identity (lambda (o) (make-array nil :initial-element o)))
	     output)))

(defun generate-selection-form (form space)
  "Generate a selection form for use with selective-assignment, i.e. (3↑x)←5."
  (let ((value-symbol) (set-form) (choose-unpicked)
	(value-placeholder (gensym)))
    (labels ((val-wssym (s)
	       (or (symbolp s)
		   (and (listp s) (eql 'inws (first s))
			(symbolp (second s)))))
	     (sfun-aliased (symbol)
	       (let ((alias-entry (get-workspace-alias space symbol)))
		 (and (symbolp symbol)
		      (characterp alias-entry)
		      (member alias-entry '(#\↑ #\↓ #\/ #\⊃) :test #'char=))))
	     (atin-aliased (symbol)
	       (let ((alias-entry (get-workspace-alias space symbol)))
		 (and (symbolp symbol)
		      (characterp alias-entry)
		      (char= #\⌷ alias-entry))))
	     (disc-aliased (symbol)
	       (let ((alias-entry (get-workspace-alias space symbol)))
		 (and (symbolp symbol)
		      (characterp alias-entry)
		      (char= #\⊃ alias-entry))))
	     (process-form (f)
	       (match f ((list* 'apl-call fn-symbol fn-form first-arg rest)
			 ;; recursively descend through the expression in search of an expression containing
			 ;; the variable and one of the four functions usable for selective assignment
			 (if (and (listp first-arg) (eql 'apl-call (first first-arg)))
			     (let ((processed (process-form first-arg)))
			       ;; only build the recursive expression if a valid expression
			       ;; was successfully found inside
			       (if processed `(apl-call ,fn-symbol ,fn-form ,processed ,@rest)))
			     (if (or (eql fn-symbol '⌷)
				     (atin-aliased fn-symbol))
				 ;; assigning to a [⌷ at index] form is an just an alternate version
				 ;; of assigning to axes, like x[1;3]←5
				 (let ((form-copy (copy-list fn-form)))
				   (setf (second form-copy)
					 (append (second form-copy) (list value-placeholder))
					 (third form) form-copy
					 set-form (fourth form))
				   form)
				 ;; set the :modify-input parameter so the array is changed in place
				 (progn (if (or (member fn-symbol '(↑ ↓ / ⊃))
						(sfun-aliased fn-symbol))
					    (if (val-wssym first-arg)
						(setq value-symbol first-arg)
						(if (and (eql 'achoose (first first-arg))
							 (val-wssym (second first-arg)))
						    (if (or (eql '⊃ fn-symbol)
							    (disc-aliased fn-symbol))
							(setq value-symbol first-arg
							      set-form
							      (append first-arg
								      (list :set value-placeholder
									    :modify-input t)))
							(setq value-symbol (second first-arg)
							      choose-unpicked t)))))
					(if value-symbol
					    `(apl-call ,fn-symbol ,fn-form
						       ,(if (not choose-unpicked)
							    value-placeholder
							    (append (list 'achoose value-placeholder)
								    (cddr first-arg)))
						       ,@rest)))))))))
      (let ((form-out (process-form form)))
	(values form-out value-symbol value-placeholder set-form)))))

(defun assign-selected (array indices values)
  "Assign array values selected using one of the functions [↑ take], [↓ drop], [\ expand] or [⊃ pick]."
  (if (or (= 0 (rank values))
	  (and (= (rank indices) (rank values))
	       (loop :for i :in (dims indices) :for v :in (dims values) :always (= i v))))
      ;; if the data to be assigned is not a scalar value, a new array must be created to ensure
      ;; that the output array will be compatible with all assigned and original values
      (let* ((to-copy-input (not (and (= 0 (rank values))
				      (or (eq t (element-type array))
					  (and (listp (type-of array))
					       (or (eql 'simple-vector (first (type-of array)))
						   (and (eql 'simple-array (first (type-of array)))
							(typep values (second (type-of array))))))))))
	     (output (if (not to-copy-input)
			 array (make-array (dims array) :element-type (if (/= 0 (rank values))
									  t (assign-element-type values)))))
	     (assigned-indices (if to-copy-input (make-array (size array) :element-type '(unsigned-byte 8)
							     :initial-element 0))))
	;; TODO: is assigning bits slow?
	;; iterate through the items to be assigned and, if an empty array has been initialized for
	;; the output, store the indices that have been assigned to new data
	(ydotimes output (i (size indices))
	  (setf (row-major-aref output (row-major-aref indices i))
		(if (= 0 (rank values)) values (row-major-aref values i)))
	  (if to-copy-input (setf (aref assigned-indices (row-major-aref indices i)) 1)))
	;; if the original array was assigned to just return it, or if a new array was created
	;; iterate through the old array and copy the non-assigned data to the output
	(if to-copy-input (xdotimes output (i (size array))
			    (if (= 0 (aref assigned-indices i))
				(setf (row-major-aref output i) (row-major-aref array i)))))
	output)
      (error "Area of array to be reassigned does not match shape of values to be assigned.")))

(defun match-lexical-function-identity (glyph)
  "Find the identity value of a lexical function based on its character."
  (second (assoc glyph `((#\+ 0) (#\- 0) (#\× 1) (#\÷ 1) (#\⋆ 1) (#\* 1) (#\! 1)
			 (#\< 0) (#\≤ 1) (#\= 1) (#\≥ 1) (#\> 0) (#\≠ 0) (#\| 0)
			 (#\^ 1) (#\∧ 1) (#\∨ 0) (#\⊤ 0) (#\∪ #()) (#\⌽ 0) (#\⊖ 0)
			 (#\⌈ ,most-negative-long-float) (#\⌊ ,most-positive-long-float))
		 :test #'char=)))

(defun operate-reducing (function function-glyph axis &optional last-axis)
  "Reduce an array along a given axis by a given function, returning function identites when called on an empty array dimension. Used to implement the [/ reduce] operator."
  (lambda (omega &optional alpha)
    (if (not (arrayp omega))
	omega (if (= 0 (size omega))
		  (or (and (= 1 (rank omega))
			   (match-lexical-function-identity (aref function-glyph 0)))
		      (make-array (loop :for i below (1- (rank omega)) :collect 0)))
		  (reduce-array omega function axis last-axis alpha)))))

(defun operate-scanning (function axis &optional last-axis inverse)
  "Scan a function across an array along a given axis. Used to implement the [\ scan] operator with an option for inversion when used with the [⍣ power] operator taking a negative right operand."
  (lambda (omega)
    (if (not (arrayp omega))
	omega (let* ((odims (dims omega))
		     (axis (or axis (if (not last-axis) 0 (1- (rank omega)))))
		     (rlen (nth axis odims))
		     (increment (reduce #'* (nthcdr (1+ axis) odims)))
		     (output (make-array odims)))
		(dotimes (i (size output)) ;; xdo
		  (declare (optimize (safety 1)))
		  (let ((value)	(vector-index (mod (floor i increment) rlen)))
		    (if inverse
			(let ((original (disclose (row-major-aref
						   omega (+ (mod i increment)
							    (* increment vector-index)
							    (* increment rlen
							       (floor i (* increment rlen))))))))
			  (setq value (if (= 0 vector-index)
					  original
					  (funcall function original
						   (disclose
						    (row-major-aref
						     omega (+ (mod i increment)
							      (* increment (1- vector-index))
							      (* increment rlen
								 (floor i (* increment rlen))))))))))
			(loop :for ix :from vector-index :downto 0
			   :do (let ((original (row-major-aref
						omega (+ (mod i increment) (* ix increment)
							 (* increment rlen (floor i (* increment rlen)))))))
				 (setq value (if (not value) (disclose original)
						 (funcall function value (disclose original)))))))
		    (setf (row-major-aref output i) value)))
		output))))

(defun operate-each (function-monadic function-dyadic)
  "Generate a function applying a function to each element of an array. Used to implement [¨ each]."
  (flet ((wrap (i) (if (not (and (arrayp i) (< 0 (rank i))))
		       i (make-array nil :initial-element i))))
    (lambda (omega &optional alpha)
      (let* ((oscalar (if (is-unitary omega) omega))
	     (ascalar (if (is-unitary alpha) alpha))
	     (odims (dims omega)) (adims (dims alpha))
	     (orank (rank omega)) (arank (rank alpha))
	     (max-rank (max orank arank)))
	(flet ((disclose-any (item)
		 (if (not (arrayp item))
		     item (row-major-aref item 0)))
	       (this-enclose (item)
		 (if (and (vectorp item)
			  (= 1 (size item)))
		     item (enclose item))))
	  (if (not (or oscalar ascalar (not alpha)
		       (and (= orank arank)
			    (loop :for da :in adims :for do :in odims :always (= da do)))))
	      (error "Mismatched left and right arguments to [¨ each].")
	      (let* ((output-dims (dims (if oscalar alpha omega)))
		     (output (make-array output-dims)))
		(if alpha (if (and oscalar ascalar)
			      (setf (row-major-aref output 0)
				    (funcall function-dyadic (disclose-any omega)
					     (disclose-any alpha)))
			      (dotimes (i (size (if oscalar alpha omega))) ;; xdo
				(setf (row-major-aref output i)
				      (funcall function-dyadic (if oscalar (disclose oscalar)
								   (row-major-aref omega i))
					       (if ascalar (disclose ascalar)
						   (row-major-aref alpha i))))))
		    ;; if 0-rank array is passed, disclose its content and enclose the result of the operation
		    (if oscalar (setq output (enclose (funcall function-monadic (disclose-any oscalar))))
			(dotimes (i (size omega)) ;; xdo
			  (setf (row-major-aref output i)
				(funcall function-monadic (row-major-aref omega i))))))
		(disclose-atom output))))))))

(defun operate-grouping (function index-origin)
  "Generate a function applying a function to items grouped by a criterion. Used to implement [⌸ key]."
  (lambda (omega &optional alpha)
    (let* ((keys (or alpha omega))
	   (key-test #'equalp)
	   (indices-of (lambda (item vector)
			 (loop :for li :below (length vector)
			    :when (funcall key-test item (aref vector li))
			    :collect (+ index-origin li))))
	   (key-table (make-hash-table :test key-test))
	   (key-list))
      (dotimes (i (size keys))
	(let ((item (row-major-aref keys i)))
	  ;; (declare (dynamic-extent item))
	  (if (loop :for key :in key-list :never (funcall key-test item key))
	      (setq key-list (cons item key-list)))
	  (setf (gethash item key-table)
		(cons (row-major-aref omega i)
		      (gethash item key-table)))))
      (let ((item-sets (loop :for key :in (reverse key-list)
			  :collect (funcall function
					    (let ((items (if alpha (gethash key key-table)
							     (funcall indices-of key keys))))
					      (make-array (length items)
							  :initial-contents (reverse items)))
					    key))))
	(mix-arrays 1 (apply #'vector item-sets))))))

(defun operate-producing-inner (right left right-glyph)
  (lambda (alpha omega)
    (array-inner-product omega alpha right left
			 (not (of-lexicon this-idiom :scalar-functions (aref right-glyph 0))))))

(defun operate-composed (right right-fn-monadic right-fn-dyadic
			 left left-fn-monadic left-fn-dyadic is-confirmed-monadic)
  "Generate a function by linking together two functions or a function curried with an argument. Used to implement [∘ compose]."
  (let ((fn-right (or right-fn-monadic right-fn-dyadic))
	(fn-left (or left-fn-monadic left-fn-dyadic)))
    (lambda (omega &optional alpha)
      (if (and fn-right fn-left)
	  (let ((processed (funcall right-fn-monadic omega)))
	    (if is-confirmed-monadic (funcall left-fn-monadic processed)
		(if alpha (funcall left-fn-dyadic processed alpha)
		    (funcall left-fn-monadic processed))))
	  (if alpha (error "This function does not take a left argument.")
	      (funcall (or right-fn-dyadic left-fn-dyadic)
		       (if (not fn-right) right omega)
		       (if (not fn-left) left omega)))))))

(defun operate-at-rank (rank function-monadic function-dyadic)
  "Generate a function applying a function to sub-arrays of the arguments. Used to implement [⍤ rank]."
  (lambda (omega &optional alpha)
    (let* ((odims (dims omega)) (adims (dims alpha))
	   (osize (size omega)) (asize (size alpha))
	   (orank (rank omega)) (arank (rank alpha))
	   (rank (if (not (arrayp rank))
		     (if (> 0 rank) ;; handle a negative rank as for ,⍤¯1⊢2 3 4⍴⍳24
			 (make-array 3 :initial-contents (list (max 0 (+ rank orank))
							       (max 0 (+ rank (if alpha arank orank)))
							       (max 0 (+ rank orank))))
			 (make-array 3 :initial-element rank))
		     (if (= 1 (size rank))
			 (make-array 3 :initial-element (row-major-aref rank 0))
			 (if (= 2 (size rank))
			     (make-array 3 :initial-contents (list (aref rank 1) (aref rank 0) (aref rank 1)))
			     (if (= 3 (size rank))
				 rank (if (or (< 1 (rank rank)) (< 3 (size rank)))
					  (error "Right operand of [⍤ rank] must be a scalar integer or ~a"
						 "integer vector no more than 3 elements long.")))))))
	   (ocrank (aref rank 2))
	   (acrank (aref rank 1))
	   (omrank (aref rank 0))
	   (orankdelta (- orank (if alpha ocrank omrank)))
	   (odivs (if (<= 0 orankdelta) (make-array (subseq odims 0 orankdelta))))
	   (odiv-dims (if odivs (subseq odims orankdelta)))
	   (odiv-size (if odivs (reduce #'* odiv-dims)))
	   (arankdelta (- arank acrank))
	   (adivs (if (and alpha (<= 0 arankdelta))
		      (make-array (subseq adims 0 arankdelta))))
	   (adiv-dims (if adivs (subseq adims arankdelta)))
	   (adiv-size (if alpha (reduce #'* adiv-dims))))
      (flet ((generate-divs (div-array ref-array div-dims div-size)
      	       (xdotimes div-array (i (size div-array))
      		 (setf (row-major-aref div-array i)
      		       (if (= 0 (rank div-array)) ref-array
			   (if (not div-dims) (row-major-aref ref-array i)
      			       (make-array div-dims :element-type (element-type ref-array)
      					   :displaced-to ref-array :displaced-index-offset (* i div-size))))))))
      	(if odivs (generate-divs odivs omega odiv-dims odiv-size))
      	(if alpha (progn (if adivs (generate-divs adivs alpha adiv-dims adiv-size))
			 (let ((output (make-array (dims (or odivs adivs)))))
			   (dotimes (i (size output)) ;; xdo
			     (let ((this-odiv (if (not odivs)
						  omega (if (= 0 (rank odivs))
							    (aref odivs) (row-major-aref odivs i))))
				   (this-adiv (if (not adivs)
						  alpha (if (= 0 (rank adivs))
							    (aref adivs) (row-major-aref adivs i)))))
			       (setf (row-major-aref output i)
				     (disclose (funcall function-dyadic this-odiv this-adiv)))))
			   output
			   (mix-arrays (max (rank odivs) (rank adivs))
			    	       output)))
	    (let ((output (make-array (dims odivs))))
	      (dotimes (i (size output)) ;; xdo
		(setf (row-major-aref output i) (funcall function-monadic (row-major-aref odivs i))))
	      (mix-arrays (rank output) output)))))))

(defun operate-atop (right-fn-monadic right-fn-dyadic left-fn-monadic)
  "Generate a function applying two functions to a value in succession. Used to implement [⍤ atop]."
  (lambda (omega &optional alpha)
    (if alpha (funcall left-fn-monadic (funcall right-fn-dyadic omega alpha))
	(funcall left-fn-monadic (funcall right-fn-monadic omega)))))

(defun operate-to-power (get-power function-retriever)
  "Generate a function applying a function to a value and successively to the results of prior iterations a given number of times. Used to implement [⍣ power]."
  (lambda (omega &optional alpha)
    (let* ((arg omega) (power (funcall get-power))
           (function (funcall function-retriever alpha (> 0 power))))
      (dotimes (index (abs power))
	(setq arg (if alpha (funcall function arg alpha)
		      (funcall function arg))))
      arg)))

(defun operate-until (op-right op-left-monadic op-left-dyadic)
  "Generate a function applying a function to a value and successively to the results of prior iterations until a condition is net. Used to implement [⍣ power]."
  (lambda (omega &optional alpha)
    (declare (ignorable alpha))
    (let ((arg omega) (prior-arg omega))
      (loop :for index :from 0 :while (or (= 0 index)
					  (= 0 (funcall op-right prior-arg arg)))
	 :do (setq prior-arg arg
		   arg (if alpha (funcall op-left-dyadic arg alpha)
			   (funcall op-left-monadic arg))))
      arg)))

(defun operate-at (left right left-fn-m left-fn-d right-fn index-origin)
  "Generate a function applying a function at indices in an array specified by a given index or meeting certain conditions. Used to implement [@ at]."
  (lambda (omega &optional alpha)
    (declare (ignorable alpha))
    (if (and left (not (functionp left)))
    	(setq left-fn-d nil left-fn-m nil))
    (if (and (or left-fn-d left-fn-m)
	     (or right-fn (or (vectorp right) (not (arrayp right)))))
	(if (and (not right-fn)
		 (not (vectorp omega)))
	    ;; if the right operand is a vector or scalar and the right argument is an array of rank > 1,
	    ;; split it into its major cells as enumerated by the right operand elements
	    (let* ((to-process (split omega 1))
		   (output-length (size to-process)))
	      (if (vectorp right)
		  (dotimes (i (length right)) ;; ydo
		    (setf (aref to-process (- (aref right i) index-origin))
			  (if alpha (funcall left-fn-d (aref to-process (- (aref right i) index-origin)
							     alpha))
			      (funcall left-fn-m (aref to-process (- (aref right i) index-origin))))))
		  (setf (aref to-process (- right index-origin))
			(if alpha (funcall left-fn-d (aref to-process (- right index-origin) alpha))
			    (funcall left-fn-m (aref to-process (- right index-origin))))))
	      (mix-arrays (max 1 (1- (rank (aref to-process 0))))
			  to-process))
	    ;; if the right operand is a function, collect the right argument's matching elements
	    ;; into a vector, apply the left operand function to it and assign its elements to their
	    ;; proper places in the copied right argument array and return it
	    (let ((true-indices (make-array (size omega) :element-type '(unsigned-byte 8) :initial-element 0))
		  (omega-copy (copy-array omega :element-type t))
		  (sv-length 0))
	      (dotimes (i (size omega)) ;; xdo
		(if (or (and right-fn (/= 0 (funcall right-fn (row-major-aref omega i))))
			(and (arrayp right)
			     (not (loop :for r :below (size right) :never (= (row-major-aref right r)
									     ;; (row-major-aref omega i)
                                                                             (+ i index-origin))))))
		    (incf (row-major-aref true-indices i))
		    (if (and (integerp right) (= i (- right index-origin)))
			(incf (row-major-aref true-indices i)))))
	      (let* ((tvix 0)
		     (true-vector (make-array (loop :for i :across true-indices :summing i)
					      :element-type (element-type omega))))
		(dotimes (i (size omega))
		  (if (/= 0 (row-major-aref true-indices i))
		      (progn (setf (row-major-aref true-vector tvix)
				   (row-major-aref omega i))
			     (incf (row-major-aref true-indices i) tvix)
			     (incf tvix))))
		(let ((to-assign (if alpha (funcall left-fn-d true-vector alpha)
				     (funcall left-fn-m true-vector))))
		  (dotimes (i (size omega)) ;; xdo 
	      	    (if (/= 0 (row-major-aref true-indices i))
	      		(setf (row-major-aref omega-copy i)
	      		      (if (= 1 (length true-vector))
				  ;; if there is only one true element the to-assign value is
				  ;; the value to be assigned, not a vector of values to assign
				  (disclose to-assign)
				  (row-major-aref to-assign (1- (row-major-aref true-indices i)))))))
		  omega-copy))))
	;; if the right argument is an array of rank > 1, assign the left operand values or apply the
	;; left operand function as per choose or reach indexing
	(nth-value
	 1 (choose omega (if (not right-fn)
			     (append (list (apl-call - (scalar-function -) right 1))
				     (loop :for i :below (1- (rank omega)) :collect nil)))
		   :set (if (not (or left-fn-m left-fn-d)) left)
		   :set-by (if (or left-fn-m left-fn-d right-fn)
			       (lambda (old &optional new)
				 (declare (ignorable new))
				 (if (and right-fn (= 0 (funcall right-fn old)))
				     old (if (not (or left-fn-m left-fn-d))
					     new (if alpha (funcall left-fn-d old alpha)
						     (funcall left-fn-m old)))))))))))

(defun operate-stenciling (right-value left-function)
  "Generate a function applying a function via (aplesque:stencil) to an array. Used to implement [⌺ stencil]."
  (lambda (omega)
    (flet ((iaxes (value index) (loop :for x :below (rank value) :for i :from 0
				   :collect (if (= i 0) index nil))))
      (if (not (or (and (< 2 (rank right-value))
			(error "The right operand of [⌺ stencil] may not have more than 2 dimensions."))
		   (and (not left-function)
			(error "The left operand of [⌺ stencil] must be a function."))))
	  (let ((window-dims (if (not (arrayp right-value))
				 (vector right-value)
				 (if (= 1 (rank right-value))
				     right-value (choose right-value (iaxes right-value 0)))))
		(movement (if (not (arrayp right-value))
			      (vector 1)
			      (if (= 2 (rank right-value))
				  (choose right-value (iaxes right-value 1))
				  (make-array (length right-value) :element-type 'fixnum
					      :initial-element 1)))))
	    (mix-arrays (rank omega)
			(stencil omega left-function window-dims movement)))))))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Aplesque -*-
;;;; obsolete.lisp

(in-package #:aplesque)

;; an archive of old implementations, not as fast as current ones but historically interesting

(defun catenate (a1 a2 axis)
  "Join two arrays along the specified axis."
  (let* ((rank1 (rank a1)) (rank2 (rank a2))
	 (max-rank (max rank1 rank2)) (uneven (/= rank1 rank2))
	 (offset (if (and uneven (< rank1 rank2)) 1 (nth axis (dims a1))))
	 (output-type (type-in-common (if (arrayp a1) (element-type a1) (assign-element-type a1))
				      (if (arrayp a2) (element-type a2) (assign-element-type a2)))))
    (if (and (> axis (1- max-rank))
	     (not (and (= 0 axis max-rank))))
	(error "Invalid axis (~a) for array with ~a dimensions." (1+ axis) max-rank))
    (if (loop :for a :in (list a1 a2) :always (= 0 (rank a)))
	(make-array 2 :element-type output-type :initial-contents (list a1 a2))
	find the shape of the output using the higher-rank array as reference
	(let ((output (make-array (destructuring-bind (ref-array second-array)
				      (funcall (if (>= rank1 rank2) #'identity #'reverse)
					       (list a1 a2))
				    (if (and uneven (arrayp second-array)
					     (< 1 (abs (- rank1 rank2))))
					(error "Catenated arrays must be at most one rank apart.")
					iterate through the reference array dimensions
					and increment the axis dimension by one (if uneven) or the
					axis dimension of the other array if both are the same rank
					(loop :for i :from 0 :for rdim :in (dims ref-array)
					   :collect (if (= i axis)
							(+ rdim (if uneven 1 (nth i (dims second-array))))
							(if (or (not (dims second-array))
								(= rdim (nth (- i (if (and uneven (> i axis))
										      1 0))
									     (dims second-array))))
							    rdim (error "Mismatched array dimensions."))))))
				  :element-type output-type)))
	  (across output (lambda (elem coords)
			   (declare (ignore elem))
			   (let* ((first-array)
				  (in-coords (loop :for x :from 0 :for c :in coords
						:when (and (= x axis)
							   (if (setq first-array (< c offset))
							       (not (arrayp a1)) (not (arrayp a2))))
						:do (return nil)
						:when (not (and uneven (= x axis)
								(if first-array (< rank1 rank2)
								    (> rank1 rank2))))
						:collect (if (or first-array (/= x axis))
							     c (- c offset))))
				  (in-array (if first-array a1 a2)))
	  		     (setf (apply #'aref output coords)
	  			   (if (not in-coords)
				       in-array (apply #'aref in-array in-coords))))))
	  output))))


(defun section (input dimensions &key (inverse nil) (populator nil))
  "Take a subsection of an array of the same rank and given dimensions as per APL's ↑ function, or invert the function as per APL's ↓ function to take the elements of an array excepting a specific dimensional range."
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
								      (copy-array prototype))))
	    output))
      (if (and (not inverse) (= 0 (loop :for d :across dimensions :summing (abs d) :into r :finally (return r))))
	  ;; in the case of a 0-size take of the array, append the remaining dimensions to the vector of zeroes
	  ;; passed to the function to create the empty output array
	  (make-array (append (array-to-list dimensions)
			      (loop :for i :from (length dimensions) :to (1- (rank input))
				 :collect (nth i (dims input))))
		      :element-type (if (and (< 0 (size input)) (arrayp (row-major-aref input 0)))
					(element-type (aref (row-major-aref input 0)))
					(assign-element-type (row-major-aref input 0))))
	  (let ((rdiff (- (rank input) (length dimensions)))
		(idims (make-array (rank input) :element-type (list 'integer 0 (size input))
				   :initial-contents (dims input))))
	    (if (< 0 rdiff)
		(setq dimensions (make-array (rank input) :element-type (list 'integer 0 (size input))
					     :initial-contents (loop :for x :below (rank input)
								  :collect (if (< x rdiff) (aref dimensions x)
									       (if inverse 0 (aref idims x))))))
		(if (> 0 rdiff)
		    (error "Too many subscripts (~w) for input array of rank ~w." (length dimensions) (rank input))))
	    (if (and inverse (loop :for x :across dimensions :for y :in (dims input) :never (> y (abs x))))
		(make-array (loop :for i :below (rank input) :collect 0))
		(let* ((fill-element (apl-array-prototype input))
		       (output (make-array (loop :for odim :across dimensions :for idim :across idims
					      :collect (if (not inverse) (abs odim) (- idim (abs odim))))
					   :element-type (if populator t (element-type input))
					   :initial-element (if (and (not populator)
								     (not (arrayp fill-element)))
								fill-element))))
		  (if populator (dotimes (i (size output)) (setf (row-major-aref output i)
								 (make-array nil :initial-element
									     (funcall populator))))
		      (if (arrayp fill-element)
			  (dotimes (i (size output)) (setf (row-major-aref output i) (copy-array fill-element)))))
		  (if (< 0 (size input))
		      (across input (lambda (element coords)
				      (setf (apply #'aref output
						   (loop :for c :in coords :for cx :from 0
						      :collect (- c (if inverse (if (> 0 (aref dimensions cx))
										    0 (aref dimensions cx))
									(if (< 0 (aref dimensions cx))
									    0 (+ (aref dimensions cx)
										 (aref idims cx)))))))
					    element))
			      :ranges (loop :for d :across dimensions :for dx :from 0
					 :collect (let ((point (if inverse (if (> 0 d) 0 d)
								   (if (< 0 d) 0 (max 0 (+ d (aref idims dx)))))))
						    (list point (if inverse
								    (+ point (- (aref idims dx)
										1 (abs (aref dimensions dx))))
								    (+ -1 point (min (abs (aref dimensions dx))
										     (aref idims dx)))))))))
		  output))))))

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
	   (output (make-array (length intervals)
			       :initial-contents (loop :for intv :in intervals
						    :collect (make-array
							      (loop :for dim :in idims :for dx :from 0
								 :collect (if (= dx axis) intv dim))
							      :element-type (if (= 0 intv)
										t (element-type input)))))))
      (dotimes (oix (size output))
	(let ((item (aref output oix)))
	  (across item (lambda (elem coords)
	  		 (declare (ignore elem))
	  		 (loop :for c :in coords :for cx :from 0
	  		    :do (setf (nth cx icoords)
	  			      (if (/= cx axis) c (+ c offset input-offset))))
	  		 (setf (apply #'aref item coords) (apply #'aref input icoords))))
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
	(let* ((output (make-array (loop :for dim :in idims :for dx :below arank
				      :collect (if (= dx axis) partitions dim))))
	       (icoords (loop :for i :below (rank input) :collect 0)))
	  (across output (lambda (elem coords)
			   (declare (ignore elem))
			   (let* ((focus (nth axis coords))
				  (this-index (nth focus indices))
				  (this-interval (nth focus intervals))
				  (out-array (make-array this-interval :element-type (element-type input))))
			     (dotimes (ix this-interval)
			       (loop :for coord :in coords :for dx :below arank :for cx :from 0
				  :do (setf (nth cx icoords)
					    (if (/= dx axis) coord (+ ix this-index))))
			       (setf (aref out-array ix) (apply #'aref input icoords)))
			     (setf (apply #'aref output coords)
				   out-array))))
	  output))))

(defun re-enclose (matrix axes)
  "Convert an array into a set of sub-arrays within a larger array. The dimensions of the containing array and the sub-arrays will be some combination of the dimensions of the original array. For example, a 2 x 3 x 4 array may be composed into a 3-element vector containing 2 x 4 dimensional arrays."
  (labels ((make-enclosure (inner-dims type dimensions)
	     (loop :for d :below (first dimensions)
		:collect (if (= 1 (length dimensions))
			     (make-array inner-dims :element-type type)
			     (make-enclosure inner-dims type (rest dimensions))))))
    (cond ((= 1 (length axes))
	   ;; if there is only one axis just split the array
	   (if (>= (aref axes 0) (rank matrix))
	       (error "Invalid axis ~w for array of rank ~w." (aref axes 0) (rank matrix))
	       (let* ((axis (aref axes 0))
		      (output (make-array (loop :for d :in (dims matrix) :for dx :from 0
					     :when (/= dx axis) :collect d))))
		 (xdotimes output (o (size output))
		   (setf (row-major-aref output o) (make-array (nth axis (dims matrix))
							       :element-type (element-type matrix))))
		 (across matrix (lambda (elem coords)
				  (let ((out-sub-array (loop :for c :in coords :for cx :from 0
							  :when (/= cx axis) :collect c))
					(out-coords (nth axis coords)))
				    (setf (aref (apply #'aref output out-sub-array) out-coords) elem))))
		 output)))
	  ((not (apply #'< (array-to-list axes)))
	   (error "Elements in an axis argument to the enclose function must be in ascending order."))
	  ((= (length axes) (rank matrix))
	   ;; if the number of axes is the same as the matrix's rank, just pass it back
	   matrix)
	  (t (let* ((outer-dims) (inner-dims)
		    (matrix-dims (dims matrix))
		    (axis-list (array-to-list axes)))
	       ;; otherwise, start by separating the dimensions of the original array into sets of dimensions
	       ;; for the output array and each of its enclosed arrays
	       (dotimes (axis (rank matrix))
		 (if (find axis axis-list) (setq inner-dims (cons axis inner-dims))
		     (setq outer-dims (cons axis outer-dims))))
	       (setq inner-dims (reverse inner-dims)
		     outer-dims (reverse outer-dims))
	       ;; create a blank array of the outer dimensions containing blank arrays of the inner dimensions
	       (let* ((ocoords (loop :for d :in outer-dims :collect (nth d matrix-dims)))
		      (icoords (loop :for d :in inner-dims :collect (nth d matrix-dims)))
		      (new-matrix (make-array ocoords :initial-contents
					      (make-enclosure icoords (element-type matrix)
							      ocoords))))
		 ;; iterate through the original array and for each element, apply the same separation
		 ;; to their coordinates that was done to the original array's dimensions and apply the two sets
		 ;; of coordinates to set each value in the nested output arrays to the corresponding values in
		 ;; the original array
		 (across matrix (lambda (item coords)
		 		  (loop :for d :in outer-dims :for dx :from 0 :do (setf (nth dx ocoords)
		 									(nth d coords)))
		 		  (loop :for d :in inner-dims :for dx :from 0 :do (setf (nth dx icoords)
		 									(nth d coords)))
		 		  (setf (apply #'aref (apply #'aref new-matrix ocoords)
		 			       icoords)
		 			item)))
		 new-matrix))))))

(defun turn (input axis &optional degrees)
  "Rotate an array on a given axis by a given number of degrees or an array containing degree values for each sub-vector."
  (if (not (arrayp input))
      input
      (let* ((idims (dims input))
	     (ocoords (loop :for i :below (rank input) :collect 0))
	     (dcoords (if (not (integerp degrees)) (loop :for i :below (1- (rank input)) :collect 0)))
	     (output (make-array idims :element-type (element-type input)))
	     (rdimension (nth axis idims)))
	(across input (lambda (item coords)
			(let ((degree (if (integerp degrees)
					  degrees (if degrees (let ((dcix 0))
								(loop :for coord :in coords
								   :for this-axis :from 0
								   :when (/= axis this-axis)
								   :do (setf (nth dcix dcoords) coord
									     dcix (1+ dcix)))
								(apply #'aref degrees dcoords))))))
			  (loop :for coord :in coords :for this-axis :from 0
			     :do (setf (nth this-axis ocoords)
				       (if (or (/= axis this-axis)
					       (and degree (= 0 degree)))
					   coord (if degree (mod (- coord degree) rdimension)
						     (- rdimension 1 coord)))))
			  (setf (apply #'aref output ocoords)
				item))))
	output)))

(defun each-scalar (function omega)
  "Iterate over an array/arrays of scalar values, operating upon them and returning the output in the most efficiently-stored array capable of holding said output."
  (let ((type)
	(output (make-array (dims omega))))
    (if (not (arrayp omega))
	omega (progn (across omega (lambda (elem coords)
				     (declare (dynamic-extent elem coords))
				     (let ((result (if (eq t function)
						       elem (funcall function elem coords))))
				       (if type (if (not (typep result type))
						    (setq type (type-in-common type (assign-element-type result))))
					   (setq type (assign-element-type result)))
				       (if coords (setf (apply #'aref output coords) result)
					   (setf (aref output) result))
				       ;; output second value to halt traversal of output array;
				       ;; if the type will be t and no change is being made to the array values,
				       ;; and thus the function value is t as well, end the traversal
				       ;; since there's no longer a point
				       (values nil (and (eq t type)
							(eq t function))))))
		     (if (eq t type)
			 (if (eq t function)
			     ;; if the traversal was terminated and thus the proper output is the original input,
			     ;; return that instead of the output
			     omega output)
			 (let ((true-output (make-array (dims omega) :element-type type)))
			   (across output (lambda (elem coords)
					    (declare (dynamic-extent elem coords))
					    (setf (apply #'aref true-output coords)
						  elem)))
			   true-output))))))

(defun expand (degrees input axis &key (compress-mode) (populator))
  "Expand an input array as per a vector of degrees, with the option to manifest zero values in the degree array as zeroes in the output in place of the original input values or to omit the corresponding values altogether if the :compress-mode option is used."
  (cond ((= 0 (size input))
	 (if compress-mode (error "An empty array cannot be compressed.")
	     (if (or (arrayp degrees)
		     (not (> 0 degrees)))
		 (error "An empty array can only be expanded to a single negative degree.")
		 (let ((output (make-array (abs degrees)
					   :element-type (element-type input)
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
	      (/= (loop :for degree :across degrees :when (< 0 degree)
		     :counting degree :into dcount :finally (return dcount))
		  (nth axis (dims input))))
	 (error "Attempting to expand elements across array but ~a"
		"positive degrees are not equal to length of selected input axis."))
	(t (let* ((degrees (if (arrayp degrees) degrees
			       (make-array (nth axis (dims input)) :initial-element degrees)))
		  ;; TODO: is there a more elegant way to handle scalar degrees or input when both aren't scalar?
		  (c-degrees (make-array (length degrees)
					 :element-type 'fixnum :initial-element 0))
		  (positive-index-list (if (not compress-mode)
					   (loop :for degree :below (length degrees)
					      :when (< 0 (aref degrees degree)) :collect degree)))
		  (positive-indices (if positive-index-list (make-array (length positive-index-list)
									:element-type 'fixnum
									:initial-contents positive-index-list)))
		  (ocoords (loop :for i :below (rank input) :collect 0))
		  (vvx (reduce #'* (loop :for d :in (dims input) :for dx :from 0
		   	 	       :when (> dx axis) :collect d)))
		  (vint (reduce #'* (loop :for d :in (dims input) :for dx :from 0
		   	 	       :while (< dx axis) :collect d)))
		  (vvec (reduce #'* (loop :for d :in (dims input) :for dx :from 0
		   	 	       :when (/= dx axis) :collect d)))
		  (vlen (nth axis (dims input)))
		  (vllast (first (last (dims input))))
		  (vvt (reduce #'* (nthcdr (1+ axis) (dims input))))
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
		   ;; TODO: linearize this
		   (across input (lambda (elem coords)
		   		   (let* ((exc (nth axis coords))
		   			  (dx (if compress-mode exc (aref positive-indices exc)))
		   			  (this-degree (aref degrees dx)))
		   		     (dotimes (ix this-degree)
		   		       (loop :for coord :in coords :for cix :from 0
		   		       	  :do (setf (nth cix ocoords)
		   		       		    (if (not (= cix axis))
		   		       			coord (print (+ ix (if (= 0 exc)
		   		       					0 (if (= 1 (length degrees))
		   		       					      (* exc (aref c-degrees 0))
		   		       					      (aref c-degrees (1- dx)))))))))
		   		       (setf (apply #'aref output ocoords)
		   			     (if (> 0 this-degree) 0 elem)))))))
	       output)))))

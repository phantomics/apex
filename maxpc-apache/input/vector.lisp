(in-package :maxpc.input.vector)

(defstruct (index-vector (:include index))
  "Index vector."
  (vector (error "Must supply VECTOR.") :type vector :read-only t))

(defstruct (index-simple-vector (:include index-vector))
  "Index simple vector.")

(defstruct (index-simple-string (:include index-vector))
  "Index simple string.")

(defmethod make-input ((input-source vector))
  (etypecase input-source
    (simple-string (make-index-simple-string :vector input-source))
    (string        (make-index-vector :vector input-source))
    (simple-vector (make-index-simple-vector :vector input-source))
    (vector        (make-index-vector :vector input-source))))

(defmethod input-empty-p ((input index-vector))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (= (the index-position (index-position input))
     (the index-position (length (the vector (index-vector-vector input))))))

(defmethod input-first ((input index-vector))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the vector (index-vector-vector input))
        (the index-position (index-position input))))

(defmethod input-first ((input index-simple-vector))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the simple-vector (index-vector-vector input))
        (the index-position (index-position input))))

(defmethod input-first ((input index-simple-string))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (aref (the simple-string (index-vector-vector input))
        (the index-position (index-position input))))

(defmethod input-rest ((input index-vector))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-vector
   :vector (the vector (index-vector-vector input))
   :position (1+ (the index-position (index-position input)))))

(defmethod input-rest ((input index-simple-vector))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-simple-vector
   :vector (the simple-vector (index-vector-vector input))
   :position (1+ (the index-position (index-position input)))))

(defmethod input-rest ((input index-simple-string))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-index-simple-string
   :vector (the simple-string (index-vector-vector input))
   :position (1+ (the index-position (index-position input)))))

(defmethod input-element-type ((input index-vector))
  (array-element-type (index-vector-vector input)))

(defmethod input-sequence ((input index-vector) (length integer))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-array (the index-position length)
              :element-type (input-element-type input)
              :displaced-to (index-vector-vector input)
              :displaced-index-offset (index-vector-position input)))

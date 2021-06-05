(in-package :maxpc.input.stream)

(defstruct (index-stream (:include index))
  (stream (error "Must supply STREAM.") :type stream :read-only t)
  (buffer (error "Must supply BUFFER.") :type vector :read-only t))

(defparameter *chunk-size* (* 1000 1000) ; 1 Mega
  "*Description:*

   {*chunk-size*} controls the size by which the buffer used for _stream
   inputs_ grows, and the number of elements read at a time when parsing from
   _streams_ of _type_ {file-stream}.")

(defparameter *bound* nil
  "*Description:*

   {*bound*} can be set to limit the number of elements read from _stream
   inputs_ in a single call to to {parse}.")

(defparameter *element-type* nil
  "*Description:*

   {*element-type*} can be set to enforce a specific stream element type when
   reading from _stream inputs_. This can be useful when dealing with bivalent
   streams.")

(defun element-type (stream)
  (or *element-type* (stream-element-type (the stream stream))))

(defgeneric fill-buffer (buffer stream))

(defmethod fill-buffer ((buffer vector) (stream stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((next (case (element-type stream)
                (character (read-char stream nil 'eof))
                (otherwise (read-byte stream nil 'eof)))))
    (unless (eq next 'eof)
      (vector-push-extend next buffer (the fixnum *chunk-size*)))))

(defmethod fill-buffer ((buffer vector) (stream file-stream))
  (let* ((file-length (file-length stream))
         (old-buffer-size (length buffer))
         (new-buffer-size (min (+ old-buffer-size *chunk-size*) file-length)))
    (unless (= file-length old-buffer-size)
      (adjust-array buffer new-buffer-size)
      (setf (fill-pointer buffer)
            new-buffer-size)
      (setf (fill-pointer buffer)
            (read-sequence buffer stream :start old-buffer-size)))))

(defmethod make-input ((input-source stream))
  (let ((buffer (make-array 0
                            :element-type (element-type input-source)
                            :adjustable t
                            :fill-pointer t)))
    (fill-buffer buffer input-source)
    (make-index-stream :stream input-source :buffer buffer)))

(declaim (inline maybe-fill-buffer)
         (ftype (function (index-stream)) maybe-fill-buffer))
(defun maybe-fill-buffer (input)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((position (index-position input))
        (buffer (index-stream-buffer input))
        (stream (index-stream-stream input)))
    (unless (> (the index-position (length (the vector buffer)))
               (the index-position position))
      (fill-buffer buffer stream)))
  (values))

(defmethod input-empty-p ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (maybe-fill-buffer input)
  (= (the index-position (index-position input))
     (the index-position
          (if *bound*
              (min #1=(length (the vector (index-stream-buffer input)))
                   (the index-position *bound*))
              #1#))))

(defmethod input-first  ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (maybe-fill-buffer input)
  (aref (the vector (index-stream-buffer input))
        (the index-position (index-position input))))

(defmethod input-rest  ((input index-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((position (index-position input))
        (buffer (index-stream-buffer input))
        (stream (index-stream-stream input)))
    (let ((next-position (1+ (the index-position position))))
      (make-index-stream :stream (the stream stream)
                         :buffer (the vector buffer)
                         :position (the index-position next-position)))))

(defmethod input-element-type ((input index-stream))
  (element-type (the stream (index-stream-stream input))))

(defmethod input-sequence ((input index-stream) (length integer))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-array (the index-position length)
              :element-type (input-element-type input)
              :displaced-to (index-stream-buffer input)
              :displaced-index-offset (index-position input)))

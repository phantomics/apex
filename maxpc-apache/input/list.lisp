(in-package :maxpc.input.list)

(defstruct (index-list (:include index))
  "Index list."
  (list (error "Must supply LIST.") :type list :read-only t))

(defmethod make-input ((input-source list))
  (make-index-list :list input-source))

(defmethod input-empty-p ((input index-list))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (null (the list (index-list-list input))))

(defmethod input-first ((input index-list))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (car (the list (index-list-list input))))

(defmethod input-rest ((input index-list))
  (declare (optimize (speed 3) (debug 0)))
  (let ((next-position (1+ (the index-position (index-position input)))))
    (check-type next-position index-position)
    (make-index-list :list (cdr (the list (index-list-list input)))
                     :position (the index-position next-position))))

(defmethod input-sequence ((input index-list) (length integer))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (subseq (the list (index-list-list input)) 0 length))

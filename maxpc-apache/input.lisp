(in-package :maxpc.input)


;;; Input interface

(defgeneric make-input (input-source)
  (:documentation
   "*Arguments and Values:*

    _input-source_—an _object_.

    *Description:*

    {make-input} returns an _input_ for _input-source_."))

(defgeneric input-empty-p (input)
  (:documentation
   "→ _empty-p_

    *Arguments and Values:*

    _input_—an _input_.

    _empty-p_—a _generalized boolean_.

    *Description:*

    {input-empty-p} returns _true_ if _input_ is empty."))

(defgeneric input-first (input)
  (:documentation
   "→ _element_

    *Arguments and Values:*

    _input_—a non-empty _input_.

    _element_—an _object_ of the _type_ designated by the _type specifier_
    returned by {input-element-type} when called on _input_.

    *Description:*

    {input-first} returns the first element in _input_.

    *Exceptional Situations:*

    If _input_ is empty the behavior of {input-first} is unspecified."))

(defgeneric input-rest (input)
  (:documentation
   "→ _rest_

    *Arguments and Values:*

    _input_—a non-empty _input_.

    _rest_—the remaining _input_.

    *Description:*

    {input-rest} returns the remaining _input_ without the first element.

    *Exceptional Situations:*

    If _input_ is empty the behavior of {input-rest} is unspecified."))

(defgeneric input-position (input)
  (:documentation
   "→ _position_

    *Arguments and Values:*

    _input_—an _input_.

    _position_—an _integer_ between 0 and {array-dimension-limit} inclusively.

    *Description:*

    {input-position} returns the _position_ of _input_.")
  (:method ((input t))
    (declare (ignore input))
    0))

(defgeneric input-element-type (input)
  (:documentation
   "→ _typespec_

    *Arguments and Values:*

    _input_—an _input_.

    _typespec_—a _type specifier_.

    *Description:*

    {input-element-type} returns a _type specifier_ that designates the _type_
    of the elements in _input_.")
  (:method ((input t))
    (declare (ignore input))
    t))

(defgeneric input-sequence (input length)
  (:documentation
   "→ _sequence_

    *Arguments and Values:*

    _input_—an _input_.

    _length_—an _integer_ between 0 and {array-dimension-limit} inclusively.

    _sequence_—a _sequence_.

    *Description:*

    {input-sequence} returns a _sequence_ of the next _length_ elements in
    _input_.

    *Exceptional Situations:*

    If the number of elements in _input_ are less than _length_ the behavior of
    {input-sequence} is unspecified.")
  (:method ((input t) (length integer))
    (loop for i from 1 to length
          for rest = input then (input-rest rest)
       collect (input-first rest))))

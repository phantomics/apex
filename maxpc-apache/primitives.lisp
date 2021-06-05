;;;; Primitive parsers and combinators—axioms, so to speak.

(in-package :maxpc)

(defun ?end  ()
  "*Description:*

   {?end} matches the end of input.

   *Examples:*

   #code#
   (parse '() (?end)) → NIL, T, T
   (parse '(a) (?end)) → NIL, NIL, NIL
   #"
  (lambda (input)
    (when (input-empty-p input)
      input)))

(defun =element ()
  "*Description:*

   {=element} matches the next element and produces that element it as its
   result value.

   *Examples:*

   #code#
   (parse '(a) (=element)) → A, T, T
   (parse '() (=element)) → NIL, NIL, T
   #"
  (lambda (input)
    (unless (input-empty-p input)
      (values (input-rest input) (input-first input) t))))

(defmacro ?fail (&body forms)
  "*Arguments and Values:*

   _forms_—_forms_.

   *Description:*

   {?fail} always fails to match, and evaluates _forms_ when it is applied.

   *Examples:*

   #code#
   (parse '(a b c) (?fail (format t \"Position: ~a~%\"
                                  (get-input-position))))
   ▷ Position: 0
   → NIL, NIL, NIL
   #"
  `(lambda (*input-fail*) ,@forms nil))

(defun ?satisfies (test &optional (parser (=element)))
  "*Arguments and Values:*

   _test_—a _designator_ for a _function_ of one argument that returns a
   _generalized boolean_.

   _parser_—a _parser_. The default is {(=element)}.

   *Description:*

   {?satisfies} matches _parser_ if its result value _satisfies the test_.

   *Examples:*

   #code#
   (parse '(1) (?satisfies 'numberp)) → NIL, T, T
   (parse '(a) (?satisfies 'numberp)) → NIL, NIL, NIL
   (parse '(a b c)
          (?satisfies (lambda (s)
                        (intersection s '(b c d)))
                      (%any (=element))))
   ⇒ NIL, T, T
   #"
  (lambda (input)
    (multiple-value-bind (rest value) (funcall parser input)
      (when (and rest (funcall test value))
        rest))))

(defun =subseq (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {=subseq} matches _parser_, and produces the subsequence matched by _parser_
   as its result value.

   *Examples:*

   #code#
   (parse '(1 2 3) (=subseq (%any (?satisfies 'numberp))))
   → (1 2 3), T, T
   (parse \"123\" (=subseq (%any (?satisfies 'digit-char-p))))
   → \"123\" T, T
   #"
  (lambda (input)
    (let ((rest (funcall parser input)))
      (when rest
        (values rest
                (input-sequence
                 input (- (input-position rest) (input-position input)))
                t)))))

(defun ?seq (&rest parsers)
  "*Arguments and Values:*

   _parsers_—_parsers_.

   *Description:*

   {?seq} matches _parsers_ in sequence.

   *Examples:*

   #code#
   (parse '(a) (?seq (=element) (?end)))
   → NIL, T, T
   (parse '(a b) (?seq (=element) (?end)))
   → NIL, NIL, NIL
   (parse '(a) (?seq))
   → NIL, T, NIL
   #"
  (lambda (input)
    (loop for parser in parsers
         do (setf input (funcall parser input))
         unless input return nil
         finally (return input))))

(defun =list (&rest parsers)
  "*Arguments and Values:*

   _parsers_—_parsers_.

   *Description:*

   {=list} matches _parsers_ in sequence, and produces a _list_ of the result
   values of _parsers_ as its result value.

   *Examples:*

   #code#
   (parse '(a) (=list (=element) (?end)))
   → (A NIL), T, T
   (parse '(a b) (=list (=element) (?end)))
   → NIL, NIL, NIL
   (parse '(a) (=list))
   → NIL, T, NIL
   #"
  (lambda (input)
    (loop for parser in parsers
       for value =
         (multiple-value-bind (rest value) (funcall parser input)
           (unless rest (return))
           (setf input rest)
           value)
       collect value into list
       finally (return (values input list t)))))

(defun %any (parser)
  "*Arguments and Values:*

   _parser_—a _parser_.

   *Description:*

   {%any} matches _parser_ in sequence any number of times. If _parser_
   produces a result value and matches at least once then {%any} produces a
   _list_ of the values as its result value.

   *Examples:*

   #code#
   (parse '(a b c) (%any (=element))) → (A B C), T, T
   (parse '() (%any (=element))) → NIL, T, T
   #"
  (lambda (input)
    (let (rest value present-p)
      (loop do (setf (values rest value present-p) (funcall parser input))
         if rest do (setf input rest)
         else return (values input list (not (null list)))
         when present-p collect value into list))))

;; Set union
(defun %or (&rest parsers)
  "*Arguments and Values:*

   _parsers_—_parsers_.

   *Description:*

   {%or} attempts to successfully apply _parsers_, and matches the first
   succeeding _parser_, if any. If that _parser_ produces a result value then
   {%or} produces that value as its result value. It can be said that {%or}
   forms the set union of _parsers_.

   *Examples:*

   #code#
   (parse '(a) (%or (?eq 'a) (?eq 'b))) → NIL, T, T
   (parse '(b) (%or (?eq 'a) (?eq 'b))) → NIL, T, T
   (parse '(a) (%or)) → NIL, NIL, NIL
   (parse '(a) (%or (=element)
                    (?fail (format t \"No element.~%\"))))
   → A, T, T
   (parse '() (%or (?fail (princ 'foo))
                   (?fail (princ 'bar))
                   (?fail (princ 'baz))))
   ▷ FOOBARBAZ
   → NIL, NIL, T
   #"
  (lambda (input)
    (loop for parser in parsers do
         (multiple-value-bind (rest value present-p) (funcall parser input)
           (when rest
             (return (values rest value present-p)))))))

;; Set intersection
(defun %and (&rest parsers)
  "*Arguments and Values:*

   _parsers_—_parsers_.

   *Description:*

   {%and} applies _parsers_, and matches the last _parser_ only if all previous
   _parsers_ succeed. If the last _parser_ produces a result value then {%and}
   produces that value as its result value. It can be said that {%and} forms
   the set intersection of _parsers_.

   *Examples:*

   #code#
   (parse '(:foo) (%and (?satisfies 'symbolp)
                        (?satisfies 'keywordp)))
   → NIL, T, T
   (parse '(foo) (%and (?satisfies 'symbolp)
                       (?satisfies 'keywordp)))
   → NIL, NIL, NIL
   (parse '(foo) (%and))
   → NIL, NIL, NIL
   (parse '(foo) (%and (?satisfies 'symbolp)
                       (=element)))
   → FOO, T, T
   (parse '() (%and (%maybe (?fail (princ 'foo)))
                    (%maybe (?fail (princ 'bar)))
                    (%maybe (?fail (princ 'baz)))))
   ▷ FOOBARBAZ
   → NIL, T, T
   #"
  (lambda (input)
    (let (rest value present-p)
      (loop for parser in parsers do
           (setf (values rest value present-p) (funcall parser input))
         unless rest return nil
         finally (return (values rest value present-p))))))

;; Set difference
(defun %diff (parser &rest not-parsers)
  "*Arguments and Values:*

   _parser_—a _parser_.

   _not‑parsers_—_parsers_.

   *Description:*

   {%diff} matches _parser_ only if applying _not‑parsers_ fails. If _parser_
   produces a result value then {%diff} produces that value as its result
   value. It can be said that {%diff} forms the set difference of _parser_ and
   the union of _not‑parsers_.

   *Examples:*

   #code#
   (parse '(foo) (%diff (?satisfies 'symbolp)
                        (?satisfies 'keywordp)))
   → NIL, T, T
   (parse '(:foo) (%diff (?satisfies 'symbolp)
                         (?satisfies 'keywordp)))
   → NIL, NIL, NIL
   (parse '(foo) (%diff (?satisfies 'symbolp)))
   → NIL, T, T
   (parse '(:foo) (%diff (?satisfies 'symbolp)))
   → NIL, T, T
   #"
  (let ((punion (apply '%or not-parsers)))
    (lambda (input)
      (unless (funcall punion input)
        (funcall parser input)))))

(defun =transform (parser function)
  "*Arguments and Values:*

   _parser_—a _parser_.

   _function_—a _designator_ for a _function_ of one argument.

   *Description:*

   {=transform} matches _parser_ and produces the result of applying _function_
   to the result value of _parser_ as its result value.

   *Examples:*

   #code#
   (parse '(41) (=transform (=element) '1+)) → 42, T, T
   (parse '() (=transform (=element) '1+)) → NIL, NIL, T
   #"
  (lambda (input)
    (multiple-value-bind (rest value) (funcall parser input)
      (when rest
        (values rest (funcall function value) t)))))

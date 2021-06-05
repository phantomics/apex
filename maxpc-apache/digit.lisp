;;;; Parsers for digit numerals.

(in-package :maxpc.digit)

(defun ?digit (&optional (radix 10))
  "*Arguments and Values:*

   _radix_—a _number_ of _type_ {(integer 2 36)}. The default is {10}.

   *Description:*

   {?digit} matches a single digit _character_ in the specified _radix_.

   *Exceptional Situations:*

   If the next element is not a _character_ an _error_ of _type_ {type-error}
   is signaled."
  (?test ('digit-char-p radix)))

(defun =natural-number (&optional (radix 10))
  "*Arguments and Values:*

   _radix_—a _number_ of _type_ {(integer 2 36)}. The default is {10}.

   *Description:*

   {=natural-number} matches one or more digit _characters_ in the specified
   _radix_ in sequence, and produces the natural _number_ represented by that
   digit sequence as its result value.

   *Examples:*

   #code#
   (parse \"234\" (=natural-number 2)) → NIL, NIL, NIL
   (parse \"101010\" (=natural-number 2)) → 42, T, T
   #

   *Exceptional Situations:*

   If an element attempted to be matched is not a _character_ an _error_ of
   _type_ {type-error} is signaled."
  (=transform (=subseq (%some (?digit radix)))
              (lambda (digits)
                (parse-integer digits :radix radix))))

(defun =integer-number (&optional (radix 10))
  "*Arguments and Values:*

   _radix_—a _number_ of _type_ {(integer 2 36)}. The default is {10}.

   *Description:*

   {=integer-number} matches one or more digit _characters_ in the specified
   _radix_, optionally lead by a sign character, in sequence, and produces the
   _integer_ represented by that sequence as its result value. The leading sign
   can be {#\\\\+} and {#\\\\-} for positive and negative values respectively.
   The default is a positive value.

   *Examples:*

   #code#
   (parse \"101010\" (=integer-number 2)) → 42, T, T
   (parse \"+101010\" (=integer-number 2)) → 42, T, T
   (parse \"-101010\" (=integer-number 2)) → -42, T, T
   (parse \"x101010\" (=integer-number 2)) → NIL, NIL, NIL
   #

   *Exceptional Situations:*

   If an element attempted to be matched is not a _character_ an _error_ of
   _type_ {type-error} is signaled."
  (=destructure (sign number)
      (=list (%maybe (%and (?test ('member '(#\+ #\-))) (=element)))
             (=natural-number radix))
    (ecase sign
      (#\- (- number))
      ((#\+ nil) number))))

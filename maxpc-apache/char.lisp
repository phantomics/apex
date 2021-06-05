;;;; Parsers for character inputs.

(in-package :maxpc.char)

(defun ?char (char &optional (case-sensitive-p t))
  "*Arguments and Values:*

   _char_—a _character_.

   _case‑sensitive‑p_—a _generalized boolean_. The default is _true_.

   *Description:*

   {?char} matches _char_. {?char} is case sensitive unless _case‑sensitive‑p_
   is _false_.

   *Exceptional Situations:*

   If the next element is not a _character_ an _error_ of _type_ {type-error}
   is signaled."
  (if case-sensitive-p
      (?test ('char= char))
      (?test ('char-equal char))))

(defun ?string (string &optional (case-sensitive-p t))
  "*Arguments and Values:*

   _string_—a _string_.

   _case‑sensitive‑p_—a _generalized boolean_. The default is _true_.

   *Description:*

   {?string} matches the _characters_ in _string_ in sequence. {?string} is
   case sensitive unless _case‑sensitive‑p_ is _false_.

   *Exceptional Situations:*

   If an element attempted to be matched is not a _character_ an _error_ of
   _type_ {type-error} is signaled."
  (apply '?seq (loop for char across string collect
                    (?char char case-sensitive-p))))

(defparameter *whitespace* '(#\Tab #\Newline #\Vt #\Ff #\Return #\Space)
  "*Value Type:*

   a _list_ of _characters_.

   *Description:*

   The _value_ of {*whitespace*} is a _list_ of _characters_ considered
   to be _whitespace characters_.")

(defun ?whitespace ()
  "*Description:*

   {?whitespace} matches an element that is a member of {*whitespace*}.

   *Exceptional Situations:*

   If the next element is not a _character_ an _error_ of _type_ {type-error}
   is signaled."
  (?test ('member *whitespace* :test 'char=)))

(defun ?newline ()
  "*Description:*

   {?newline} matches the {#\\\\Newline} _character_."
  (?char #\Newline))

(defun =line (&optional keep-newline-p)
  "*Arguments and Values:*

   _keep‑newline‑p_—a _generalized boolean_. The default is _false_.

   *Description:*

   {=line} matches zero or more _characters_ in sequence followed by a
   {#\\\\Newline} _character_ or the end of input, and produces the _string_ of
   _characters_ as its result value. The terminating {#\\\\Newline} _character_
   is not included in _string_ unless _keep‑newline‑p_ is _true_.

   *Examples:*

   #code#
   (parse (format nil \"foo~%bar~%baz\") (%any (=line)))
   → (\"foo\" \"bar\" \"baz\"), T, T
   #

   *Exceptional Situations:*

   If an element attempted to be matched is not a _character_ an _error_ of
   _type_ {type-error} is signaled."
  (=destructure (line _)
      (%or (=list (=subseq (%any (?not (?newline)))) (?newline))
           (=list (=subseq (%some (?not (?end)))) (?end)))
    (if keep-newline-p
        (format nil "~a~%" #1=(coerce line 'string))
        #1#)))

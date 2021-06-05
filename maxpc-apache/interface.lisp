(in-package :maxpc)

;;; Parser interface is...
;;;
;;;    (FUNCTION (INPUT) INPUT * BOOLEAN)
;;;
;;; ...where the first return value is the remaining input, the second return
;;; value is the parser’s “result value” or NIL and the third return value is a
;;; boolean that indicates if a result value is present.

(defvar *input-start*)

(defvar *input-fail*)

(defun parse (input-source parser)
  "→ _result_, _match‑p_, _end‑p_

   *Arguments and Values:*

   _input-source_—an _input source_.

   _parser_—a parser.

   _result_—an _object_.

   _match‑p_, _end‑p_—_generalized booleans_.

   *Description:*

   {parse} applies _parser_ to the _input_ and returns the parser’s _result
   value_ or {nil}. _Match‑p_ is _true_ if _parser_ _matched_ the
   _input-source_. _End‑p_ is _true_ if _parser_ _matched_ the complete
   _input-source_. _Input_ is derived from _input-source_ by using
   {maxpc.input:make-input}.

   {parse} accepts _input sources_ of _type_ {sequence} and {stream} out of the
   box.

   *See Also:*

   [input](#section-4), [maxpc.input.stream](#section-5)"
  (let ((*input-start* (make-input input-source)))
    (multiple-value-bind (rest value) (funcall parser *input-start*)
      (values value
              (not (null rest))
              (or (input-empty-p *input-start*)
                  (and rest (input-empty-p rest)))))))

(defun parse-line-position (input position)
  "Parses line position of POSITION in INPUT."
  (loop for i from 0 to position
     for in = input then (input-rest in)
     for end-p = (input-empty-p in)
     for newline-p = (unless end-p
                       (char= #\Newline (input-first in)))
     for character = 0 then (if newline-p 0 (1+ character))
     for line = 1 then (+ line (if newline-p 1 0))
     when (or (= i position) end-p) return (values line character)))

(defun get-input-position ()
  "→ _position_

   → _position_, _line_, _column_

   *Arguments and Values:*

   _position_, _column_—non-negative _integers_.

   _line_—a positive _integer_.

   *Description:*

   {get-input-position} returns the number of elements read from the input so
   far. Additionally, _line_ and _column_ positions are returned if the input's
   _element type_ is {character}. Lines are counted starting at one while
   columns are counted starting from zero.

   {get-input-position} may only be called from within the body of {?fail}, the
   handlers of {%handler-case} or the restarts of {%restart-case}.

   *Exceptional Situations:*

   If {get-input-position} is not evaluated within the dynamic context of
   {?fail}, {%handler-case} or {%restart-case} an _error_ of _type_
   {simple-error} is signaled."
  (unless *input-fail*
    (error "GET-INPUT-POSITION may only be called inside ?FAIL, %HANDLER-CASE
and %RESTART-CASE."))
  (let ((position (input-position *input-fail*)))
    (if (eq (input-element-type *input-fail*) 'character)
	(multiple-value-bind (line character)
	    (parse-line-position *input-start* position)
	  (values position line character))
	position)))

(defun cases-to-parser-cases (cases input-sym)
  "Utility macro function for %HANDLER-CASE and %RESTART-CASE."
  (loop for case in cases collect
       (destructuring-bind (typespec lambda-list &rest forms) case
         `(,typespec ,lambda-list
                     ,@(butlast forms)
                     (funcall ,@(last forms) ,input-sym)))))

(defmacro %handler-case (parser &body clauses
                         &aux (input-sym (gensym "input"))
                              (parser-sym (gensym "parser")))
  "_clauses_::= {{}↓_error‑clause_{\\}}\\*

   _error‑clause_::=
     {(}_typespec_ {([}_var_{])}
        {{}_declaration_{\\}}\\* {{}_form_{\\}}\\* _parser‑form_{)}

   *Arguments and Values:*

   _parser_—a _parser_.

   _typespec_—a _type specifier_.

   _var_—a _variable name_.

   _declaration_—a {declare} _expression_; not evaluated.

   _form_—a _form_.

   _parser‑form_—a _form_ that evaluates to a _parser_.

   *Description:*

   {%handler-case} executes _parser_ in a _dynamic environment_ where handlers
   are active as if by {handler-case}. If a _condition_ is handled by
   {%handler-case}, _parser‑form_ is evaluated and the resulting _parser_ is
   applied.

   *Examples:*

   #code#
   (defun assert-digit (c)
     (or (digit-char-p c)
         (error \"Not a digit: ~c\" c)))

   (parse \"01x2\"
          (%any (%handler-case (%and (?satisfies 'assert-digit)
                                     (=element))
                  (error (e)
                    (format t \"Error at position ~a: ~a~%\"
                            (get-input-position) e)
                    (?seq (=element))))))
   ▷ Error at position 2: Not a digit: x
   → (#\\0 #\\1 #\\2), T, T
   #

   *See Also:*

   {handler-case}."
  `(let ((,parser-sym ,parser))
     (lambda (,input-sym)
       (let ((*input-fail* ,input-sym))
         (handler-case (funcall ,parser-sym ,input-sym)
           ,@(cases-to-parser-cases clauses input-sym))))))

(defmacro %restart-case (parser &rest clauses
                         &aux (input-sym (gensym "input"))
                              (parser-sym (gensym "parser")))
  "_clauses_::= {{}↓_restart‑clause_{\\}}\\*

   _restart‑clause_::=
     {(}_case‑name_ {([}_lambda‑list_{])}
     〚{:interactive} _interactive‑expression_ |
       {:report} _report‑expression_ |
       {:test} _test‑expression_〛
     {{}_declaration_{\\}}\\* {{}_form_{\\}}\\* _parser‑form_{)}

   *Arguments and Values:*

   _parser_—a _parser_.

   _case‑name_—a _symbol_ or {nil}.

   _lambda‑list_—an _ordinary lambda list_.

   _interactive‑expression_—a _symbol_ or a _lambda expression_.

   _report‑expression_—a _string_, a _symbol_, or a _lambda expression_.

   _test‑expression_—a _symbol_ or a _lambda expression_.

   _declaration_—a {declare} _expression_; not evaluated.

   _form_—a _form_.

   _parser‑form_—a _form_ that evaluates to a _parser_.

   *Description:*

   {%restart-case} executes _parser_ in a _dynamic environment_ where restarts
   are active as if by {restart-case}. If control is transferred to a
   _restart‑clause_, _parser‑form_ is evaluated and the resulting _parser_ is
   applied.

   *Examples:*

   #code#
   (parse \"012x3\"
          (%any (%restart-case
                    (=transform
                     (=element)
                     (lambda (c)
                       (if (digit-char-p c)
                           c
                           (error \"Not a digit: ~c\" c))))
                  (skip-element ()
                    :report \"Skip character.\"
                    (?seq (=element))))))
   ▷ Error: Not a digit: x
   ▷ To continue, type :CONTINUE followed by an option number:
   ▷  1: Skip non-digit character.
   ▷  2: Return to Lisp Toplevel.
   ▷ Debug> :continue 1
   ▷ Invoking restart: Skip character.
   → (#\\0 #\\1 #\\2), T, T
   #

   *See Also:*

   {restart-case}."
  `(let ((,parser-sym ,parser))
     (lambda (,input-sym)
       (let ((*input-fail* ,input-sym))
         (restart-case (funcall ,parser-sym,input-sym)
           ,@(cases-to-parser-cases clauses input-sym))))))

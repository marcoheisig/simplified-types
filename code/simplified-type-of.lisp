(in-package #:simplified-types)

(defgeneric simplified-type-of (object)
  (:documentation
   "Returns a simplified type specifier for a type that has OBJECT as an
element.

The expression (simplified-type-of OBJECT) yields exactly the same result
as the expression (simplify-type (type-of OBJECT)), but the former is
likely to be more efficient."))

(defmethod simplified-type-of ((object t))
  't)

(defmethod simplified-type-of ((integer integer))
  `(integer ,integer ,integer))

(defmacro compile-time-when (test &body body)
  (when (eval test) `(progn ,@body)))

(compile-time-when (find-class 'short-float nil)
  (defmethod simplified-type-of ((short-float short-float))
    'short-float))

(compile-time-when (find-class 'single-float nil)
  (defmethod simplified-type-of ((single-float single-float))
    'single-float))

(compile-time-when (find-class 'double-float nil)
  (defmethod simplified-type-of ((double-float double-float))
    'double-float))

(compile-time-when (find-class 'long-float nil)
  (defmethod simplified-type-of ((long-float long-float))
    'long-float))

(defmethod simplified-type-of ((complex complex))
  (locally (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
    (typecase complex
      ((complex double-float) '(complex double-float))
      ((complex long-float) '(complex long-float))
      ((complex single-float) '(complex single-float))
      ((complex short-float) '(complex short-float))
      (t 't))))

(defmethod simplified-type-of ((function function))
  'function)

(defmethod simplified-type-of ((character character))
  'character)

(defmethod simplified-type-of ((package package))
  'package)

(defmethod simplified-type-of ((symbol symbol))
  'symbol)

(defmethod simplified-type-of ((cons cons))
  'cons)

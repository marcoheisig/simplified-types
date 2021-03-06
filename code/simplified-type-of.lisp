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
  (make-integer-type integer integer))

(defmacro enable-if-class-exists (class &body body)
  (if (find-class class nil)
      `(progn ,@body)
      `(progn)))

(enable-if-class-exists short-float
  (defmethod simplified-type-of ((short-float short-float))
    +short-float-type+))

(enable-if-class-exists single-float
  (defmethod simplified-type-of ((single-float single-float))
    +single-float-type+))

(enable-if-class-exists double-float
  (defmethod simplified-type-of ((double-float double-float))
    +double-float-type+))

(enable-if-class-exists long-float
  (defmethod simplified-type-of ((long-float long-float))
    +long-float-type+))

(defmethod simplified-type-of ((complex complex))
  (locally (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
    (typecase complex
      ((complex double-float) +complex-double-float-type+)
      ((complex long-float) +complex-long-float-type+)
      ((complex single-float) +complex-single-float-type+)
      ((complex short-float) +complex-short-float-type+)
      (t 't))))

(defmethod simplified-type-of ((function function))
  'function)

(defmethod simplified-type-of ((character character))
  'character)

(defmethod simplified-type-of ((symbol symbol))
  'symbol)

(defmethod simplified-type-of ((cons cons))
  'cons)

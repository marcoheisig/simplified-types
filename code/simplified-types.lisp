(in-package #:simplified-types)

(deftype simplified-floating-point-type-specifier ()
  '(member short-float single-float double-float long-float))

(deftype simplified-complex-type-specifier ()
  '(cons
    (eql complex)
    (cons simplified-floating-point-type-specifier null)))

(deftype simplified-integer-type-specifier ()
  '(cons (eql integer)
    (cons (or (eql *) integer)
     (cons (or (eql *) integer)
      null))))

(deftype simplified-type-specifier ()
  '(or
    (member t function character package symbol cons nil)
    simplified-integer-type-specifier
    simplified-complex-type-specifier
    simplified-floating-point-type-specifier))

(defvar *precise-integer-types* t
  "Whether the lower- and upper-limit of simplified integer type specifiers
should be as accurate as possible, or whether it is permissible that one or
both of them can be upgraded to the symbol *.

While the value of this variable is false, working with simplified types is
guaranteed to be non-consing.")

(defun make-integer-type (lower-limit upper-limit)
  (if *precise-integer-types*
      `(integer ,lower-limit ,upper-limit)
      '(integer * *)))

(define-compiler-macro make-integer-type (&whole whole lower-limit upper-limit)
  (if (and (constantp lower-limit)
           (constantp upper-limit))
      `(load-time-value `(integer ,,lower-limit ,,upper-limit) t)
      whole))

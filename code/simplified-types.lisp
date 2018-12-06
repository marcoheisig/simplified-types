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
    (member t function base-char extended-char package symbol cons nil)
    simplified-integer-type-specifier
    simplified-complex-type-specifier
    simplified-floating-point-type-specifier))

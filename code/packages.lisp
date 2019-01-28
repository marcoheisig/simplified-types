(cl:in-package #:common-lisp-user)

(defpackage #:simplified-types
  (:use #:common-lisp)
  (:export
   #:simplified-type-specifier
   #:simplified-number-type-specifier
   #:simplified-integer-type-specifier
   #:simplified-complex-type-specifier
   #:simplified-floating-point-type-specifier
   #:simplify-type
   #:simplified-type-of))

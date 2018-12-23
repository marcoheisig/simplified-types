(cl:in-package #:common-lisp-user)

(cl:defpackage #:simplified-types
  (:use #:common-lisp)
  (:export
   #:simplified-type-specifier
   #:simplify-type
   #:simplified-type-of))

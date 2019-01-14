(in-package #:simplified-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simplified Floating-Point Type Specifiers

(defmacro define-floating-point-types ()
  (let ((floating-point-types
          (remove-duplicates
           (list 'short-float 'single-float 'long-float 'double-float)
           :test #'alexandria:type=)))
    (flet ((find-type (type)
             (or (find type floating-point-types :test #'alexandria:type=)
                 (error "Invalid floating point type: ~S." type))))
      `(progn
         (deftype simplified-floating-point-type-specifier ()
           '(member ,@floating-point-types))
         (alexandria:define-constant +short-float-type+ ',(find-type 'short-float))
         (alexandria:define-constant +single-float-type+ ',(find-type 'single-float))
         (alexandria:define-constant +double-float-type+ ',(find-type 'double-float))
         (alexandria:define-constant +long-float-type+ ',(find-type 'long-float))))))

(define-floating-point-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simplified Complex Type Specifiers

(defmacro define-complex-types ()
  (let ((complex-types
          (remove-duplicates
           (list '(complex short-float)
                 '(complex single-float)
                 '(complex long-float)
                 '(complex double-float))
           :test #'alexandria:type=)))
    (flet ((find-type (type)
             (or (find type complex-types :test #'alexandria:type=)
                 (error "Invalid complex type: ~S." type))))
      `(progn
         (deftype simplified-complex-type-specifier ()
           '(cons
             (eql complex)
             (cons (or ,@(loop for (nil type) in complex-types collect `(eql ,type)))
              null)))
         (alexandria:define-constant +complex-short-float-type+ ',(find-type '(complex short-float)) :test 'equal)
         (alexandria:define-constant +complex-single-float-type+ ',(find-type '(complex single-float)) :test 'equal)
         (alexandria:define-constant +complex-double-float-type+ ',(find-type '(complex double-float)) :test 'equal)
         (alexandria:define-constant +complex-long-float-type+ ',(find-type '(complex long-float)) :test 'equal)))))

(define-complex-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simplified Integer Type Specifiers

(deftype simplified-integer-type-specifier ()
  '(cons (eql integer)
    (cons (or (eql *) integer)
     (cons (or (eql *) integer)
      null))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Simplified Type Specifiers

(deftype simplified-type-specifier ()
  '(or
    (member t function character package symbol cons nil)
    simplified-integer-type-specifier
    simplified-complex-type-specifier
    simplified-floating-point-type-specifier))

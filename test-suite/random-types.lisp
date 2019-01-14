(in-package #:simplified-types-test-suite)

(defmacro one-of (&body choices)
  `(ecase (random ,(length choices))
     ,@(loop for choice in choices
             for n from 0
             collect `(,n ,choice))))

(defun random-type-specifier ()
  (one-of
    (random-atomic-type-specifier)
    (random-number-type-specifier)
    (random-array-type-specifier)
    (random-cons-type-specifier)
    (random-compound-only-type-specifier)))

(defun random-atomic-type-specifier ()
  (one-of
    'arithmetic-error                  'function            'simple-condition
    'array                             'generic-function    'simple-error
    'atom                              'hash-table          'simple-string
    'base-char                         'integer             'simple-type-error
    'base-string                       'keyword             'simple-vector
    'bignum                            'list                'simple-warning
    'bit                               'logical-pathname    'single-float
    'bit-vector                        'long-float          'standard-char
    'broadcast-stream                  'method              'standard-class
    'built-in-class                    'method-combination  'standard-generic-function
    'cell-error                        'nil                 'standard-method
    'character                         'null                'standard-object
    'class                             'number              'storage-condition
    'compiled-function                 'package             'stream
    'complex                           'package-error       'stream-error
    'concatenated-stream               'parse-error         'string
    'condition                         'pathname            'string-stream
    'cons                              'print-not-readable  'structure-class
    'control-error                     'program-error       'structure-object
    'division-by-zero                  'random-state        'style-warning
    'double-float                      'ratio               'symbol
    'echo-stream                       'rational            'synonym-stream
    'end-of-file                       'reader-error        't
    'error                             'readtable           'two-way-stream
    'extended-char                     'real                'type-error
    'file-error                        'restart             'unbound-slot
    'file-stream                       'sequence            'unbound-variable
    'fixnum                            'serious-condition   'undefined-function
    'float                             'short-float         'unsigned-byte
    'floating-point-inexact            'signed-byte         'vector
    'floating-point-invalid-operation  'simple-array        'warning
    'floating-point-overflow           'simple-base-string
    'floating-point-underflow          'simple-bit-vector))

(defun random-number-type-specifier ()
  (one-of
    (random-complex-type-specifier)
    (random-real-type-specifier)))

(defun random-complex-type-specifier ()
  (one-of
   'complex
   '(complex)
   '(complex *)
   `(complex ,(random-real-type-specifier))))

(defun random-integer-type-specifier ()
  (flet ((random-integer ()
           (one-of
             -5 -2 -1 0 1 2 5
             most-positive-fixnum
             (1+ most-positive-fixnum)
             (1- most-positive-fixnum)
             most-negative-fixnum
             (1+ most-negative-fixnum)
             (1- most-negative-fixnum))))
    (let* ((a (random-integer))
           (b (random-integer))
           (lower-bound (min a b))
           (upper-bound (max a b)))
      (one-of
        'integer
        'unsigned-byte
        'signed-byte
        '(integer)
        '(unsigned-byte)
        '(signed-byte)
        `(unsigned-byte ,(one-of (1+ (random 64)) '*))
        `(signed-byte ,(one-of (1+ (random 64)) '*))
        `(mod ,(1+ (random 4)))
        `(integer ,(one-of lower-bound (list lower-bound) '*))
        `(integer ,(one-of lower-bound (list lower-bound) '*)
                  ,(one-of upper-bound (list upper-bound) '*))))))

(defun random-real-type-specifier ()
  (flet ((random-real-of-type (type)
           (coerce (- (random 7) 3) type)))
    (let* ((type (one-of 'float 'rational 'real 'single-float 'short-float 'double-float 'long-float))
           (a (random-real-of-type type))
           (b (random-real-of-type type))
           (lower-bound (min a b))
           (upper-bound (max a b)))
      (one-of
        type
        'ratio
        `(,type ,(one-of lower-bound (list lower-bound) '*))
        `(,type ,(one-of lower-bound (list lower-bound) '*))
        `(,type ,(one-of lower-bound (list lower-bound) '*)
                ,(one-of upper-bound (list lower-bound) '*))))))

(defun random-array-type-specifier ()
  (one-of
   (let ((base-type (one-of 'array 'simple-array)))
     (one-of
      base-type
      `(,base-type)
      `(,base-type ,(one-of (random-type-specifier) '*))
      `(,base-type ,(one-of (random-type-specifier) '*)
                   ,(one-of '* (random 8) (loop repeat (random 12) collect (one-of '* (random 8)))))))
   (let ((base-type (one-of 'base-string 'string 'simple-string
                            'simple-bit-vector 'bit-vector 'simple-vector)))
     (one-of
      base-type
      `(,base-type)
      `(,base-type ,(random 8))))
   (one-of
    'vector
    `(vector)
    `(vector ,(one-of '* (random-type-specifier)))
    `(vector ,(one-of '* (random-type-specifier))
             ,(one-of '* (random 8))))))

(defun random-cons-type-specifier ()
  (one-of
    'cons
    '(cons)
    `(cons ,(one-of (random-type-specifier) '*))
    `(cons ,(one-of (random-type-specifier) '*)
           ,(one-of (random-type-specifier) '*))))

(defun random-compound-only-type-specifier ()
  (one-of
    `(not ,(random-type-specifier))
    `(or ,@(loop repeat (random 4) collect (random-type-specifier)))
    `(and ,@(loop repeat (random 4) collect (random-type-specifier)))
    `(eql ,(one-of 42 "foo" #() nil -0.0d0 1.0 5/8))
    `(satisfies ,(one-of 'oddp 'evenp 'integerp 'functionp 'characterp 'values))))

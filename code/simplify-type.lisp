(in-package #:simplified-types)

(trivia:defpattern floating-point-type-specifier (type)
  `(or ',type
       (list ',type)
       (list ',type
             (or (type ,type) (list (type ,type)) '*))
       (list ',type
             (or (type ,type) (list (type ,type)) '*)
             (or (type ,type) (list (type ,type)) '*))))

(defun simplify-type (type-specifier &optional environment)
  "Returns a simplified type specifier that is a supertype of TYPE-SPECIFIER.

In particular, for any type specifier TS, the expression
 (subtypep TS (simplify-type TS)) will evaluate to either T T, or NIL NIL."
  (declare (optimize (compilation-speed 0)))
  (flet ((fail () (error "Invalid type specifier: ~A" type-specifier)))
    (trivia:match type-specifier
      ;; Unsigned integer types.
      ('bit
       (make-integer-type 0 1))
      ((or 'unsigned-byte (list 'unsigned-byte) (list 'unsigned-byte '*))
       (make-integer-type 0 '*))
      ((list 'unsigned-byte (and n (type (integer 1 *))))
       (make-integer-type 0 (1- (expt 2 n))))
      ((list 'mod (and n (type (integer 1 *))))
       (make-integer-type 0 (1- n)))
      ;; Signed integer types.
      ((or 'signed-byte (list 'signed-byte) (list 'signed-byte '*))
       (make-integer-type '* '*))
      ('fixnum
       (make-integer-type most-negative-fixnum most-positive-fixnum))
      ((list 'signed-byte (and n (type (integer 1 *))))
       (let ((2^n (expt 2 n)))
         (make-integer-type (1- (- 2^n)) (- 2^n 2))))
      ;; Interval integer types.
      ((or 'integer (list 'integer)) (make-integer-type '* '*))
      ((or (and (list 'integer lower-bound) (trivia:<> upper-bound '*))
           (list 'integer lower-bound upper-bound))
       (let ((simplified-lower-bound
               (typecase lower-bound
                 ((eql *) '*)
                 (integer lower-bound)
                 ((cons integer null) (1+ (car lower-bound)))
                 (otherwise (fail))))
             (simplified-upper-bound
               (typecase upper-bound
                 ((eql *) '*)
                 (integer upper-bound)
                 ((cons integer null) (1- (car upper-bound)))
                 (otherwise (fail)))))
         (if (and (integerp simplified-lower-bound)
                  (integerp simplified-upper-bound)
                  (not (<= simplified-lower-bound simplified-upper-bound)))
             'nil
             (make-integer-type simplified-lower-bound simplified-upper-bound))))
      ;; Cons types.
      ((or (and (or 'cons (list 'cons)) (trivia:<> car-type '*) (trivia:<> cdr-type '*))
           (and (list 'cons car-type) (trivia:<> cdr-type '*))
           (and (list 'cons car-type cdr-type)))
       (flet ((empty-type-p (type)
                (and (not (eq type '*))
                     (null (simplify-type type)))))
         (if (or (empty-type-p car-type)
                 (empty-type-p cdr-type))
             'nil
             'cons)))
      ;; Floating point types.
      ((floating-point-type-specifier short-float) +short-float-type+)
      ((floating-point-type-specifier single-float) +single-float-type+)
      ((floating-point-type-specifier double-float) +double-float-type+)
      ((floating-point-type-specifier long-float) +long-float-type+)
      ;; Complex types.
      ((or 'complex (list 'complex) (list 'complex '*)) 't)
      ((list 'complex type)
       (case (simplify-type type)
         (short-float +complex-short-float-type+)
         (single-float +complex-single-float-type+)
         (double-float +complex-double-float-type+)
         (long-float +complex-long-float-type+)
         (otherwise 't)))
      ;; Logical connectives.
      ((list 'not type) (if (eq type 't) 'nil 't))
      ((list 'or) 'nil)
      ((list 'and) 't)
      ((list (or 'and 'or) type) (simplify-type type))
      ((list* 'and types)
       (reduce #'simplified-type-conjunction types :key #'simplify-type))
      ((list* 'or types)
       (reduce #'simplified-type-disjunction types :key #'simplify-type))
      ;; Member types.
      ((list 'member) 'nil)
      ((list 'member object) (simplified-type-of object))
      ((list* 'member objects)
       (reduce #'simplified-type-disjunction objects :key #'simplified-type-of))
      ((list 'eql object) (simplified-type-of object))
      ;; Skip already simplified type specifiers.
      ((type simplified-type-specifier) type-specifier)
      ;; Attempt to detect malformed type specifiers.
      ((list 'satisfies predicate)
       (if (and (symbolp predicate) (fboundp predicate))
           't
           (fail)))
      ((or 'satisfies 'mod 'eql
           (list* (or 'unsigned-byte 'mod 'signed-byte 'integer 'complex
                      'short 'single-float 'double-float 'long-float
                      'not 'eql 'values 'quote)
                  _))
       (fail))
      (_
       ;; Handle function types.  These are too hairy for pattern matching.
       (if (subtypep type-specifier 'function environment)
           'function
           ;; Attempt to expand recursively.
           (multiple-value-bind (expansion expanded-p)
               (handler-case (introspect-environment:typexpand-1 type-specifier environment)
                 (error () (fail)))
             (if (not expanded-p)
                 't
                 (simplify-type expansion environment))))))))

(defun simplified-type-conjunction (t1 t2)
  (declare (type simplified-type-specifier t1 t2))
  (etypecase t1
    (symbol
     (cond ((eq t1 t2) t1)
           ((eq t1 't) t2)
           ((eq t2 't) t1)
           (t 'nil)))
    (cons
     (cond ((symbolp t2) (if (eq t2 't) t1 'nil))
           ((not (eq (first t1) (first t2))) 'nil)
           ((eq (first t1) 'integer)
            (destructuring-bind (lower-limit-1 upper-limit-1) (rest t1)
              (destructuring-bind (lower-limit-2 upper-limit-2) (rest t2)
                (let ((lower-limit
                        (cond ((eq lower-limit-1 '*) lower-limit-2)
                              ((eq lower-limit-2 '*) lower-limit-1)
                              (t (max lower-limit-1 lower-limit-2))))
                      (upper-limit
                        (cond ((eq upper-limit-1 '*) upper-limit-2)
                              ((eq upper-limit-2 '*) upper-limit-1)
                              (t (min upper-limit-1 upper-limit-2)))))
                  (cond ((or (eq lower-limit '*)
                             (eq upper-limit '*)
                             (<= lower-limit upper-limit))
                         (make-integer-type lower-limit upper-limit))
                        (t 'nil))))))
           ((eq (first t1) 'complex)
            (if (eq (second t1) (second t2)) t1 'nil))))))

(defun simplified-type-disjunction (t1 t2)
  (declare (type simplified-type-specifier t1 t2))
  (etypecase t1
    (symbol
     (cond ((eq t1 t2) t1)
           ((eq t1 'nil) t2)
           ((eq t2 'nil) t1)
           (t 't)))
    (cons
     (cond ((symbolp t2) (if (eq t2 'nil) t1 't))
           ((not (eq (first t1) (first t2))) 't)
           ((eq (first t1) 'integer)
            (destructuring-bind (lower-limit-1 upper-limit-1) (rest t1)
              (destructuring-bind (lower-limit-2 upper-limit-2) (rest t2)
                (let ((lower-limit
                        (cond ((eq lower-limit-1 '*) '*)
                              ((eq lower-limit-2 '*) '*)
                              (t (min lower-limit-1 lower-limit-2))))
                      (upper-limit
                        (cond ((eq upper-limit-1 '*) '*)
                              ((eq upper-limit-2 '*) '*)
                              (t (max upper-limit-1 upper-limit-2)))))
                  (make-integer-type lower-limit upper-limit)))))
           ((eq (first t1) 'complex)
            (if (eq (second t1) (second t2)) t1 't))))))

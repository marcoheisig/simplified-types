(in-package #:simplified-types)

(trivia:defpattern floating-point-type-specifier (type)
  (check-type type simplified-floating-point-type-specifier)
  `(or ',type
       (list ',type)
       (list ',type
             (or (type ,type) (list (type ,type)) '*))
       (list ',type
             (or (type ,type) (list (type ,type)) '*)
             (or (type ,type) (list (type ,type)) '*))))

(defun simplify-type-specifier (type-specifier)
  "Returns a simplified type specifier that is a supertype of TYPE-SPECIFIER.

In particular, for any type specifier TS, the expression
 (subtypep TS (simplify-type-specifier TS))
will evaluate to either T T, or NIL NIL."
  (flet ((fail () (error "Invalid type specifier: ~A" type-specifier)))
    (trivia:match type-specifier
      ;; Unsigned integer types.
      ('bit
       '(integer 0 1))
      ((or 'unsigned-byte (list 'unsigned-byte '*))
       '(integer 0 *))
      ((list 'unsigned-byte (and n (type (integer 1 *))))
       `(integer 0 ,(1- (expt 2 n))))
      ((list 'mod (and n (type (integer 1 *))))
       `(integer 0 ,(1- n)))
      ;; Signed integer types.
      ((or 'signed-byte (list 'signed-byte '*))
       '(integer * *))
      ((list 'signed-byte (and n (type (integer 1 *))))
       (let ((2^n (expt 2 n)))
         `(integer ,(1- (- 2^n)) ,(- 2^n 2))))
      ;; Interval integer types.
      ((list 'integer
             (and lower-limit (type integer))
             (and upper-limit (type integer)))
       (unless (<= lower-limit upper-limit)
         (fail))
       type-specifier)
      ((or
        (and (or 'integer (list 'integer))
             (trivia:<> lower-limit '*) (trivia:<> upper-limit '*))
        (and (list 'integer (and lower-limit (type integer)))
             (trivia:<> upper-limit '*))
        (list 'integer
              (and lower-limit (or (type integer) (list (type integer))))
              (and upper-limit (or (type integer) (list (type integer))))))
       (let ((lower-limit
               (typecase lower-limit
                 ((eql *) '*)
                 ((integer) lower-limit)
                 ((cons integer null) (1+ (car lower-limit)))
                 (t (fail))))
             (upper-limit
               (typecase upper-limit
                 ((eql *) '*)
                 ((integer) upper-limit)
                 ((cons integer null) (1- (car upper-limit)))
                 (t (fail)))))
         (when (and (integerp lower-limit)
                    (integerp upper-limit)
                    (< upper-limit lower-limit))
           (fail))
         `(integer ,lower-limit ,upper-limit)))
      ;; Floating point types.
      ((floating-point-type-specifier short-float) 'short-float)
      ((floating-point-type-specifier single-float) 'single-float)
      ((floating-point-type-specifier double-float) 'double-float)
      ((floating-point-type-specifier long-float) 'long-float)
      ;; Complex types.
      ((list 'complex (floating-point-type-specifier short-float)) '(complex short-float))
      ((list 'complex (floating-point-type-specifier single-float)) '(complex single-float))
      ((list 'complex (floating-point-type-specifier double-float)) '(complex double-float))
      ((list 'complex (floating-point-type-specifier long-float)) '(complex long-float))
      ((or 'complex (list 'complex type))
       (case (simplify-type-specifier type)
         (short-float '(complex short-float))
         (single-float '(complex single-float))
         (double-float '(complex double-float))
         (long-float '(complex long-float))
         (otherwise 't)))
      ;; Logical connectives.
      ((list 'not type) (if (eq type 't) 'nil 't))
      ((list 'or) 'nil)
      ((list 'and) 't)
      ((list (or 'and 'or) type) (simplify-type-specifier type))
      ((list* 'and types)
       (reduce #'simplified-type-conjunction types :key #'simplify-type-specifier))
      ((list* 'or types)
       (reduce #'simplified-type-disjunction types :key #'simplify-type-specifier))
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
       (unless (fboundp predicate)
         (fail)))
      ((or 'satisfies 'mod 'eql
        (list* (or 'unsigned-byte 'mod 'signed-byte 'integer 'complex
                   'short 'single-float 'double-float 'long-float
                   'not 'eql 'values)
               _))
       (fail))
      (_
       ;; Handle function types.  These are too hairy for pattern matching.
       (if (subtypep type-specifier 'function)
           'function
           ;; Attempt to expand recursively.
           (multiple-value-bind (expansion expanded-p)
               (handler-case (introspect-environment:typexpand-1 type-specifier)
                 (error () (fail)))
             (if (not expanded-p)
                 't
                 (simplify-type-specifier expansion))))))))

(defun simplified-type-conjunction (t1 t2)
  (declare (simplified-type-specifier t1 t2))
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
                  (cond ((or (eq lower-limit '*) (eq upper-limit '*))
                         `(integer ,lower-limit ,upper-limit))
                        ((<= lower-limit upper-limit)
                         `(integer ,lower-limit ,upper-limit))
                        (t 'nil))))))
           ((eq (first t1) 'complex)
            (if (eq (second t1) (second t2)) t1 'nil))))))

(defun simplified-type-disjunction (t1 t2)
  (declare (simplified-type-specifier t1 t2))
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
                  `(integer ,lower-limit ,upper-limit)))))
           ((eq (first t1) 'complex)
            (if (eq (second t1) (second t2)) t1 't))))))


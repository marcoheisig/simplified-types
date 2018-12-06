(in-package #:simplified-types)

(defun simplify-type-specifier (type-specifier)
  (flet ((fail () (error "Invalid type specifier: ~A" type-specifier)))
    (trivia:match type-specifier
      ;; Already simplified type specifiers.
      ((type simplified-type-specifier)
       type-specifier)
      ;; Unsigned integer types.
      ((or 'unsigned-byte (list 'unsigned-byte '*))
       '(integer 0 *))
      ((list 'unsigned-byte n)
       (unless (typep n '(integer 1 *))
         (fail))
       `(integer 0 ,(1- (expt 2 n))))
      ((list 'mod n)
       (unless (typep n '(integer 1 *))
         (fail))
       `(integer 0 ,(1- n)))
      ;; Signed integer types.
      ((or 'signed-byte (list 'signed-byte '*))
       '(integer * *))
      ((list 'signed-byte n)
       (unless (typep n '(integer 1 *))
         (fail))
       (let ((2^n (expt 2 n)))
         `(integer ,(1- (- 2^n)) ,(- 2^n 2))))
      ;; Interval integer types.
      ((or (and 'integer (trivia:<> lower-limit '*) (trivia:<> upper-limit '*))
           (and (list 'integer) (trivia:<> lower-limit '*) (trivia:<> upper-limit '*))
           (and (list 'integer lower-limit) (trivia:<> upper-limit '*))
           (and (list 'integer lower-limit upper-limit)))
       (block nil
         (flet ((simplify-lower-limit (lower-limit)
                  (typecase lower-limit
                    ((eql *) '*)
                    ((integer) lower-limit)
                    ((cons integer null) (1+ (car lower-limit)))
                    (t (fail))))
                (simplify-upper-limit (upper-limit)
                  (typecase upper-limit
                    ((eql *) '*)
                    ((integer) upper-limit)
                    ((cons integer null) (1- (car upper-limit)))
                    (t (fail)))))
           `(integer ,(simplify-lower-limit lower-limit)
                     ,(simplify-upper-limit upper-limit)))))
      ;; Floating point types.
      ((or (list* 'short-float _) 'short-float) 'short-float)
      ((or (list* 'single-float _) 'single-float) 'single-float)
      ((or (list* 'double-float _) 'double-float) 'double-float)
      ((or (list* 'long-float _) 'long-float) 'long-float)
      ;; Complex types.
      ((list 'complex (or (list* 'short-float _) 'short-float)) '(complex short-float))
      ((list 'complex (or (list* 'single-float _) 'single-float)) '(complex single-float))
      ((list 'complex (or (list* 'double-float _) 'double-float)) '(complex double-float))
      ((list 'complex (or (list* 'long-float) 'long-float)) '(complex long-float))
      ((or 'complex (list 'complex type))
       (unless (subtypep type 'real)
         (fail))
       'number)
      ;; Logical connectives.
      ((list 'not type)
       (if (eq type 't) 'nil 't))
      ((list 'or) 'nil)
      ((list 'and) 't)
      ((list (or 'and 'or) type) type)
      ((list* 'and types)
       (reduce #'simplified-type-conjunction types :key #'simplify-type-specifier))
      ((list* 'or types)
       (reduce #'simplified-type-disjunction types :key #'simplify-type-specifier))
      ;; Error handling.
      ((list* (or 'unsigned-byte 'signed-byte 'integer 'mod 'complex 'not
                  'short-float 'single-float 'double-float 'long-float)
              _)
       (fail))
      ;; Expand recursively, or return T.
      (_
       (multiple-value-bind (expansion expanded-p)
           (introspect-environment:typexpand-1 type-specifier)
         (if (not expanded-p)
             't
             (simplify-type-specifier expansion)))))))

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


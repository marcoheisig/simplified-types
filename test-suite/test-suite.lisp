(in-package #:simplified-types-test-suite)

(defun simplify-type-cautiously (type-specifier)
  (let ((result (simplify-type type-specifier)))
    (assert (typep result 'simplified-type-specifier))
    (multiple-value-bind (subtype-p valid-p)
        (subtypep type-specifier result)
      (assert (eq subtype-p valid-p))
      result)))

(defun run-test-suite ()
  (test-repeatedly #'test-arbitrary-types 10000)
  (test-repeatedly #'test-integer-types 10000)
  (test-repeatedly #'test-hairy-types 10000))

(defun test-repeatedly (test &optional max-tests)
  (if (null max-tests)
      (loop (funcall test))
      (loop repeat max-tests do (funcall test))))

(defun test-arbitrary-types ()
  (simplify-type-cautiously
   (random-type-specifier)))

(defun test-integer-types ()
  (simplify-type-cautiously
   (random-integer-type-specifier)))

(defun test-hairy-types ()
  (simplify-type-cautiously
   (random-compound-only-type-specifier)))

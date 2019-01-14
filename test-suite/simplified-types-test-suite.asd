(defsystem "simplified-types-test-suite"
  :description "Simplification of Common Lisp type specifiers. (test suite)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria")

  :perform
  (test-op (o c) (symbol-call "SIMPLIFIED-TYPES-TEST-SUITE" "RUN-TEST-SUITE"))

  :serial t
  :components
  ((:file "packages")
   (:file "random-types")
   (:file "test-suite")))

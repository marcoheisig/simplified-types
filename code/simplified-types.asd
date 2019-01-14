(defsystem "simplified-types"
  :description "Simplification of Common Lisp type specifiers."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "introspect-environment"
   "trivia")

  :in-order-to ((test-op (test-op :simplified-types-test-suite)))

  :serial t
  :components
  ((:file "packages")
   (:file "simplified-types")
   (:file "simplified-type-of")
   (:file "simplify-type")))

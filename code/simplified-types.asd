(defsystem "simplified-types"
  :description "Simplification of Common Lisp type specifiers."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("introspect-environment"
   "trivia")

  :serial t
  :components
  ((:file "packages")
   (:file "simplified-types")
   (:file "simplified-type-of")
   (:file "simplify-type")))

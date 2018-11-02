#|
  This file is a part of cl-kleuren project.
|#

(defsystem "cl-kleuren-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Cole Lyman <cole@colelyman.com>"
  :license "MIT"
  :depends-on ("cl-kleuren"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-kleuren")
                 (:test-file "colored-de-bruijn-graph"))))
  :description "Test system for cl-kleuren"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))

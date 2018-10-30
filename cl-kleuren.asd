#|
  This file is a part of cl-kleuren project.
|#

(defsystem "cl-kleuren"
  :version "0.1.0"
  :author "Cole Lyman <cole@colelyman.com>"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-kleuren")
                 (:file "colored-de-bruijn-graph"))))
  :description "Finding bubbles using the Colored de Bruijn Graph."
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.org"))
  :in-order-to ((test-op (test-op "cl-kleuren-test"))))

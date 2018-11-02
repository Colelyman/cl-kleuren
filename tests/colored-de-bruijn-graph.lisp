(defpackage colored-de-bruijn-graph-test
  (:use :cl
        :colored-de-bruijn-graph
        :prove))
(in-package :colored-de-bruijn-graph-test)

(defparameter *k*
  5)

(defparameter *cdbg-test*
  (make-instance '<colored-de-bruijn-graph> :k *k*))

(defparameter *colors*
  ())

(plan 8)

;; check accessor functions
(is (k *cdbg-test*) *k*)
(is (colors *cdbg-test*) nil)

(build-genomes *cdbg-test* "../tests/data/small/genomes.txt")

(is (get-node-colors *cdbg-test* "GCGGG") '(1 1 1 1))
(is (get-node-colors *cdbg-test* "CACAT") '(1 0 0 0))
(is (get-node-colors *cdbg-test* "CACTT") '(0 1 0 1))
(is (get-node-colors *cdbg-test* "ACACA") '(2 1 1 1))

(is (get-suffix-neighbors *cdbg-test* "GCGGG") '("CGGGG"))
(is (get-prefix-neighbors *cdbg-test* "GCGGG") nil)


(finalize)

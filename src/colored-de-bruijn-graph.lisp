(defpackage colored-de-bruijn-graph
  (:use :cl)
  (:export :<colored-de-bruijn-graph>
           :k
           :colors
           :build-genomes
           :add-node
           :get-node-colors
           :get-suffix-neighbors
           :get-prefix-neighbors
           :get-kmers
           :get-suffix
           :get-prefix))
(in-package :colored-de-bruijn-graph)

(declaim (optimize (debug 3)))

(defvar *nucleotides*
  '("A" "C" "G" "T"))

(defclass <colored-de-bruijn-graph> ()
  ((k :type integer
      :initarg :k
      :initform (error "Must supply K (kmer size).")
      :reader k
      :documentation "The kmer size, the size of each substring in the graph")
   (nodes :type hash-table
          :initform (make-hash-table :test 'equal)
          :documentation "A `hash-table' where each key is a node of length K
and the value is a list of the colors that are represented.")
   (colors :type cons
           :initform ()
           :accessor colors
           :documentation "A `plist' where each indicator is an index and the
property is the human readable name of the genome."))
  (:documentation "A Colored de Bruijn Graph (CdBG) represents a set of genomes in graph form.

The nodes of the CdBG are of substrings of the genomes of length K and an edge
exists when the K - 1 suffix of one node is the same as the K - 1 prefix of
another node and vice versa. Furthermore, each node encodes in which genome
the substring occurs."))

(defgeneric build-genomes (cdbg genome-files-path)
  (:documentation "Builds the CdBG given the files in GENOME-FILES-PATH. The contents of
GENOME-FILES-PATH should be either absolute or relative file paths separated by new lines."))

(defgeneric build-genome (cdbg genome-file-path index)
  (:documentation "Builds the CdBG given GENOME-FILE-PATH and the INDEX. The INDEX is used
to store the genome (from the file name) id in COLORS."))

(defgeneric add-node (cdbg kmer node-colors)
  (:documentation "Adds a node KMER to the NODES with NODE-COLORS."))

(defgeneric get-node-colors (cdbg kmer)
  (:documentation "Returns the colors associated with KMER if KMER is an element of NODES,
otherwise returns NIL."))

(defgeneric get-suffix-neighbors (cdbg kmer)
  (:documentation "Returns the suffix neighbors associated with KMER."))

(defgeneric get-prefix-neighbors (cdbg kmer)
  (:documentation "Returns the prefix neighbors associated with KMER."))

;; (defmethod initialize-instance :after ((cdbg <colored-de-bruijn-graph>) &rest initargs)
;;   (with-slots (colors) cdbg
;;     (let ((color-args (getf initargs :colors)))
;;       (setf colors color-args))))

(defmethod build-genomes ((cdbg <colored-de-bruijn-graph>) genome-files-path)
  (with-open-file (genomes-fh genome-files-path)
    (loop for genome-file = (read-line genomes-fh nil)
          for i from 0
          while genome-file
          do (push `(,i . ,(file-namestring genome-file)) (colors cdbg)))
    (file-position genomes-fh :start)
    (loop for genome-file = (read-line genomes-fh nil)
          for i from 0
          while genome-file
          do (build-genome cdbg genome-file i))))

(defmethod build-genome ((cdbg <colored-de-bruijn-graph>) genome-file-path color-index)
  (with-open-file (genome-fh genome-file-path)
    (with-slots (k nodes colors) cdbg
      (mapc #'(lambda (kmer) (add-node cdbg kmer color-index))
            (get-kmers (apply #'concatenate 'string
                              (loop for line = (read-line genome-fh nil)
                                    while line
                                    when (not (equal (schar line 0) #\>))
                                      collect line)) k)))))

(defmethod add-node ((cdbg <colored-de-bruijn-graph>) kmer color-index)
  (with-slots (nodes colors) cdbg
    (multiple-value-bind (existing-colors present) (gethash kmer nodes)
      (let* ((num-colors (length colors))
             (node-colors (gen-node-colors color-index num-colors)))
        (if (not present)
            (setf (gethash kmer nodes) (mapcar '+ node-colors
                                               (make-list num-colors :initial-element 0)))
            (setf (gethash kmer nodes) (mapcar '+ node-colors existing-colors)))))))

(defmethod get-node-colors ((cdbg <colored-de-bruijn-graph>) kmer)
  (with-slots (nodes) cdbg
    (multiple-value-bind (colors present) (gethash kmer nodes)
      (if present
          colors
          nil))))

(defmethod get-suffix-neighbors ((cdbg <colored-de-bruijn-graph>) kmer)
  (get-neighbors cdbg kmer
                 #'get-suffix
                 #'(lambda (kmer bp) (concatenate 'string kmer bp))))

(defmethod get-prefix-neighbors ((cdbg <colored-de-bruijn-graph>) kmer)
  (get-neighbors cdbg kmer
                 #'get-prefix
                 #'(lambda (kmer bp) (concatenate 'string bp kmer))))

(defun get-kmers (seq k)
  "Get the kmers of length K from SEQ."
  (let ((last-index (1+ (- (length seq) k))))
    (loop for i from 0 to last-index
          until (>= i last-index)
          collect (subseq seq i (+ i k)))))

(defun get-suffix (seq)
  "Get the suffix of SEQ."
  (subseq seq 1 (1- (length seq))))

(defun get-prefix (seq)
  "Get the prefix of SEQ."
  (subseq seq 0 (1- (length seq))))

(defun get-neighbors (cdbg kmer affix-fn concat-fn)
  (with-slots (nodes) cdbg
    (let ((affix (funcall affix-fn kmer))
          (neighbors ()))
      (loop for bp in *nucleotides*
            do (let ((neighbor (funcall concat-fn affix bp)))
                 (when (get-node-colors cdbg neighbor)
                   (push neighbor neighbor))))
      neighbors)))

(defun gen-node-colors (index colors-length)
  "Generates a list of length COLORS-LENGTH where all elements are 0, except
the INDEXth element is 1."
  (let ((node-colors))
    (setf node-colors (make-list colors-length :initial-element 0))
    (setf (nth index node-colors) 1)
    node-colors))

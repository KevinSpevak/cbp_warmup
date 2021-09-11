;;;; Usage: run as lisp script from command line
;;;; e.g. > sbcl --script test.lisp

(load "models.lisp")
(load "sentences.lisp")
(load "solvers.lisp")

(defun print-result (output)
  (if output
      (print-model output)
      (print "Unsatisfiable")))

(print "---Results for enum-sat (brute force)---")
(defvar sent '((P Q) (~P)))
(print "sentence: ")
(print sent)
(print "result:")
(print-result (enum-sat sent))
(print "")

(setq sent '((P Q) (~P) (~Q)))
(print "sentence: ")
(print sent)
(print "result:")
(print-result (enum-sat sent))

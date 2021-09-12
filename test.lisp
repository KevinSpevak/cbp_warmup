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

(print "---Results for dpll-sat ---")
(setq sent '((P Q) (~P)))
(print "sentence: ")
(print sent)
(print "result:")
(print-result (dpll-sat sent))
(print "")

(setq sent '((P Q) (~P) (~Q)))
(print "sentence: ")
(print sent)
(print "result:")
(print-result (dpll-sat sent))

(print "--- enum-sat/dpll-sat performance comparison")
(defvar long-easy '((A) (B ~G) (~A C) (A ~B C ~D E ~F) (D) (~B ~C F) (~G ~H I) (~G ~I ~A) (H) (~J ~G ~I) (~G D ~B J ~K) (~K ~J) (L ~M ~K) (~M ~L)))
(time (enum-sat long-easy))
(time (dpll-sat long-easy))

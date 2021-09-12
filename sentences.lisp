;;;; Utilities for representing propositional logic sentences
;;;; Unless otherwise indicated, sentences are assumed to be in Conjunctive Normal Form
;;;; CNF Sentences are represented as list of clauses, where each clause is a
;;;; list of literals, which are symbols with or without a leading ~

;;; Returns t if the literal is negated
(defun is-neg (literal) (eq (char (symbol-name literal) 0) #\~))

;;; Returns the proposition symbol of a literal
(defun lit-prop (literal)
  (if (is-neg literal)
      (read-from-string (subseq (symbol-name literal) 1))
      literal))

;;; Returns the negated literal (cancelling doulbe negation)
(defun negate (literal)
  (if (is-neg literal)
      (lit-prop literal)
      (read-from-string (format NIL "~s~s" '~ literal))))

;;; Returns t if the literal is true in the model
;;; See models.lisp
(defun true-lit (literal model)
  (if (is-neg literal)
      (and (is-assigned (lit-prop literal) model)
	   (not (gethash (lit-prop literal) model)))
      (gethash literal model)))

;;; Returns a list of proposition symbols in a sentence
(defun sentence-symbols (sent)
  (let ((symbols NIL))
    (loop for clause in sent
	  do (loop for lit in clause
		   do (setq symbols (adjoin (lit-prop lit) symbols))))
    symbols))

;;; Returns a list of proposition symbols in a sentence
(defun sentence-literals (sent)
  (let ((symbols NIL))
    (loop for clause in sent
	  do (loop for lit in clause
		   do (setq symbols (adjoin lit symbols))))
    symbols))

;;; Returns t if the model satisfies the cnf sentence
(defun mod-satisfies-sent (model sentence)
  (every (lambda (clause)
	   (some (lambda (lit)
		   (true-lit lit model))
		 clause))
	 sentence))

;;;; Utilities for representing propositional logic sentences
;;;; Unless otherwise indicated, sentences are assumed to be in Conjunctive Normal Form
;;;; CNF Sentences are represented as list of clauses, where each clause is a
;;;; list of literals, which are symbols with or without a leading ~

;;; Creates a literal from a name and a polarity
(defun literal (name polarity)
  (if polarity
      (read-from-string name)
      (read-from-string (format NIL "~a~a" '~ name))))

;;; Returns t if the literal is negated
(defun is-neg (literal) (eq (char (symbol-name literal) 0) #\~))

;;; Returns t if literal is not negated
(defun is-pos (literal) (not (is-neg literal)))

;;; Returns the proposition symbol of a literal
(defun lit-prop (literal)
  (if (is-neg literal)
      (read-from-string (subseq (symbol-name literal) 1))
      literal))

;;; Returns the negated literal (cancelling doulbe negation)
(defun negate (literal)
  (if (is-neg literal)
      (lit-prop literal)
      (read-from-string (format NIL "~a~a" '~ literal))))

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

;;; Returns a random k-CNF sentence with m clauses and up to n symbols
;;; for ease of implementation, repeated clauses is allowed
(defun rand-cnf (k m n)
  (if (> k n)
      (print "n must be greater than k")
      (let ((cnf NIL))
	(loop for i from 1 to m
	      do (setq cnf (adjoin (rand-clause k n) cnf)))
	cnf)))

;;; Returns a random clause with k literals from a pool of n symbols
(defun rand-clause (k n)
  (if (> k n)
      (print "n must be greater than k")
      (let ((clause NIL) (lit NIL))
	(loop
	  do (progn
	       (setq lit (random n))
	       (if (not (find lit clause)) (setq clause (adjoin lit clause)))
	       (if (eq (length clause) k) (return))))
	(mapcar (lambda (lit) (literal (format NIL "~a~a" 'P lit) (eq (random 2) 0))) clause))))

;;; Returns t if the sentence is a conjunction of horn clauses
(defun is-horn-cnf (sentence)
  (every (lambda (clause) (< (count-if #'is-pos clause) 2)) sentence))

;;; Returns a random sentence of m horn clauses with between 1 and k literals per clause,
;;; up to n symbols
(defun rand-horn-cnf (k m n)
  (if (> k n)
      (print "n must be greater than k")
      (let ((cnf NIL))
	(loop for i from 1 to m
	      do (setq cnf (adjoin (rand-horn-clause k n) cnf)))
	cnf)))

;;; Returns a random horn clause with up to k literals from a pool of n symbols
(defun rand-horn-clause (k n)
  (if (> k n)
      (print "n must be greater than k")
      (let ((clause NIL) (lit NIL))
	(setq k (+ 1 (random k)))
	(loop
	  do (progn
	       (setq lit (random n))
	       (if (not (find lit clause)) (setq clause (adjoin lit clause)))
	       (if (eq (length clause) k) (return))))
	(setq clause (mapcar (lambda (lit) (literal (format NIL "~a~a" 'P lit) NIL)) clause))
	(if (eq (random 2) 0)
	    (cons (negate (car clause)) (cdr clause))
	    clause))))

;;;; Implementations of various algorithms for solving SAT problems

;;; Model enumeration implementation of SAT
;;; Returns a satisfying model if one exists
;;;   sentence: a CNF sentence
(defun enum-sat (sentence)
  (enum-sat-model sentence (make-hash-table)))


;;; Subroutine for enum-sat
(defun enum-sat-model (sentence model)
  (if (every (lambda (prop) (is-assigned prop model)) (sentence-symbols sentence))
      (and (mod-satisfies-sent model sentence) model)
      (let ((prop (find-if (lambda (x) (not (is-assigned x model))) (sentence-symbols sentence))))
	(or (enum-sat-model sentence (set-prop (copy-model model) prop t))
	    (enum-sat-model sentence (set-prop (copy-model model) prop NIL))))))

;;; DPLL implementation of SAT
;;; Returns a satisfying model if one exists
;;;   sentence: a CNF sentence
;;; note: model used in this function only for tracking assignment to return
(defun dpll-sat (sentence)
  (dpll-sat-model sentence (make-hash-table)))

;;; Subroutine for dpll-sat
(defun dpll-sat-model (sentence model)
  (setq sentence (unit-propagate sentence model))
  (setq sentence (assign-pure-symbols sentence model))
  (cond ((not sentence) model) ; No clauses -> satisfied
	((some #'not sentence) NIL) ; Empty clause -> unsatisfiable
	(t (let ((prop (lit-prop (car (car sentence)))))
	     (or (enum-sat-model (cons sentence (list prop)) (set-prop (copy-model model) prop t))
		 (enum-sat-model (cons sentence (list (negate prop))) (set-prop (copy-model model) prop NIL)))))))

;;; Unit propagation for DPLL
(defun unit-propagate (sentence model)
  (loop
    (let ((unit (find-if (lambda (clause) (eq (length clause) 1)) sentence)))
      (if unit
	  (progn
	    (setq unit (car unit))
	    (setq sentence (delete-if (lambda (clause) (find unit clause)) sentence))
	    (setq sentence (mapcar (lambda (clause) (delete (negate unit) clause)) sentence))
	    (set-prop model (lit-prop unit) (not (is-neg unit))))
	  (return sentence)))))

;;; Pure symbol assignment for DPLL
(defun assign-pure-symbols (sentence model)
  (let ((literals (sentence-literals sentence)))
    (setq literals (remove-if (lambda (lit) (find (negate lit) literals)) literals))
    (loop for pure-symbol in literals
	  do (progn
	       (setq sentence (remove-if (lambda (clause) (find pure-symbol clause)) sentence))
	       (set-prop model (lit-prop pure-symbol) (not (is-neg pure-symbol)))))
    sentence))

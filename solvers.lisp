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

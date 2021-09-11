;;;; Utilities for representing propositional logic models
;;;; models are hash tables mapping symbols to truth values assigned in the model

;;; Creates a model with the given true and false proposition symbols
(defun create-model (t_syms f_syms)
  (let ((m (make-hash-table)))
    (loop for sym in t_syms
	  do (setf (gethash sym m) t))
    (loop for sym in f_syms
	  do (setf (gethash sym m) NIL))
    m))

;;; Sets the truth value of a propostion symbol in the model
(defun set-prop (model prop value)
  (setf (gethash prop model) value)
  model)

;;; Returns t if the proposition symbol is assigned in the model
(defun is-assigned (prop model)
  (nth-value 1 (gethash prop model)))

;;; Returns a list of proposition symbols in the model
(defun model-symbols (model)
  (loop for key being the hash-keys of model
	collect key))

;;; Returns a copy of the model
(defun copy-model (model)
  (let ((new-model (make-hash-table)))
    (loop for sym in (model-symbols model)
	  do (set-prop new-model sym (gethash sym model)))
    new-model))

;;; Prints the truth values of all proposition symbols
(defun print-model (model)
  (loop for key being the hash-keys of model
	  using (hash-value value)
	do (print (format NIL "~s: ~s" key (or (and value 'T) 'F)))))


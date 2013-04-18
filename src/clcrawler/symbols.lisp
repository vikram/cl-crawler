(in-package :com.crawler)

(defun symb (&rest args)
  "Make a symbol out of the printed representations of the arguments."   ; LMH
  (values (intern (apply #'mkstr args))))

(defun make-logic-symbol (string)
  "Convert string to symbol, preserving case, except for AND/OR/NOT/FORALL/EXISTS."
  (cond ((find string '(and or not forall exists) :test #'string-equal))
        ((lower-case-p (char string 0)) 
	 (symb (string-upcase string)))
	((equal string "Nil") '|Nil|)
        (t (intern (string-upcase string)))))

(defun mysymbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (format nil "~{~a~}" args)))

(defun make-keyword (string)
  (when string
    (intern (string-upcase string) :keyword)))

(defun fn-name (fn)
  (multiple-value-bind (x  y name)
      (function-lambda-expression fn)
    (symbol-name name)))

(defun keyword-name (keyword)
  (string-downcase (symbol-name keyword)))

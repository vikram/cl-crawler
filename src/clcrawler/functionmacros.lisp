(in-package :com.crawler)

;;;; ********************************************************************************
;;;; Section 15.1, Macros Returning Functions: Building Functions
;;;; ********************************************************************************

;;; fn subsumes fif, fint, fun

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns)))
                       g)))
          (rec fns)))))

(defmacro fn (expr)
  "Make the function according to the expression,
   e.g., (fn (and ingegerp oddp)) makes the function
   #'(lambda (x) (and (integerp x) (oddp x)))."   ; LMH
 `#',(rbuild expr))

(defun or-fns (&rest fns)
  #'(lambda (&rest args)
      (if (rest fns)
	  (or (apply (first fns) args)
	      (apply (apply #'or-fns (rest fns)) args))
	  (apply (first fns) args))))

(defun and-fns (&rest fns)
  #'(lambda (&rest args)
      (if (rest fns)
	  (and (apply (first fns) args)
	       (apply (apply #'and-fns (rest fns)) args))
	  (apply (first fns) args))))

(defun callfun (fn &rest args)
  (apply fn args))

(defun compose (&rest fns)
  "Compose the functions."   ; LMH
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
	(declare (function fn1))			; LMH
        #'(lambda (&rest args)
            (reduce #'callfun fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defmacro-exported defequal (name comp test &rest fns)
  (with-gensyms (x y)
    `(defun ,name (,x ,y)
       (and (,test ,x)
	    (,test ,y)
	    (,comp
	     (funcall (fn (compose ,@fns)) ,x)
	     (funcall (fn (compose ,@fns)) ,y))))))


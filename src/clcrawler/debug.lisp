(in-package :com.crawler)
;;debug stuff

(defparameter *dbg-ids* nil)
(defparameter *fail* 'fail)

(defun dbg (id format-string args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (if args
	(apply #'format *debug-io* format-string args)
	(apply #'format *debug-io* format-string (list "none found")))))

(defun debug-ids (&rest ids)
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug-ids (&rest ids)
  (setf *dbg-ids* (if (null ids) nil
		      (set-difference *dbg-ids* ids))))

(defun debug-indent (id indent format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

; Errors & Debugging

(defun ero (&rest args)
  (print (if (cdr args) args (car args))
         *error-output*))

(defmacro safely (expr)
  (with-gensyms (ret err)
    `(bind (,ret ,err) (ignore-errors ,expr)
       (if (typep ,err 'error)
           nil
           (values ,ret ,err)))))

(defmacro in-case-error (expr err)
  (with-gensyms (val cond)
    `(bind (,val ,cond) (ignore-errors ,expr)
       (if (typep ,cond 'error)
           (progn
             ,err
             (error ,cond))
           ,val))))


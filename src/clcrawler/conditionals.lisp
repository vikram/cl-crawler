(in-package :com.crawler)

(defvar if*-keyword-list '("then" "thenret" "else" "elseif"))

(defmacro if* (&rest args)
   (do ((xx (reverse args) (cdr xx))
	(state :init)
	(elseseen nil)
	(totalcol nil)
	(lookat nil nil)
	(col nil))
       ((null xx)
	(cond ((eq state :compl)
	       `(cond ,@totalcol))
	      (t (error "if*: illegal form ~s" args))))
       (cond ((and (symbolp (car xx))
		   (member (symbol-name (car xx))
			   if*-keyword-list
			   :test #'string-equal))
	      (setq lookat (symbol-name (car xx)))))

       (cond ((eq state :init)
	      (cond (lookat (cond ((string-equal lookat "thenret")
				   (setq col nil
					 state :then))
				  (t (error
				      "if*: bad keyword ~a" lookat))))
		    (t (setq state :col
			     col nil)
		       (push (car xx) col))))
	     ((eq state :col)
	      (cond (lookat
		     (cond ((string-equal lookat "else")
			    (cond (elseseen
				   (error
				    "if*: multiples elses")))
			    (setq elseseen t)
			    (setq state :init)
			    (push `(t ,@col) totalcol))
			   ((string-equal lookat "then")
			    (setq state :then))
			   (t (error "if*: bad keyword ~s"
					      lookat))))
		    (t (push (car xx) col))))
	     ((eq state :then)
	      (cond (lookat
		     (error
		      "if*: keyword ~s at the wrong place " (car xx)))
		    (t (setq state :compl)
		       (push `(,(car xx) ,@col) totalcol))))
	     ((eq state :compl)
	      (cond ((not (string-equal lookat "elseif"))
		     (error "if*: missing elseif clause ")))
	      (setq state :init)))))

(defun ifnot (bad val) 
   (unless (eql bad val) val))

(defmacro nullor (x y)
  (with-gensyms (g)
    `(let ((,g ,x))
       (if (zerop (length ,g)) ,y ,g))))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro casequal (val &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,val))
       (cond ,@(mapcar #'(lambda (cl)
                           `(,(if (eql (car cl) t)
                                  t
                                  `(equal ,g ,(car cl)))
                             ,@(cdr cl)))
                       clauses)))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;;;; ********************************************************************************
;;;; Section 14.1, Anaphoric Macros: Anaphoric Variants
;;;; ********************************************************************************

(defmacro awhen (test-form &body body)
  `(let ((it ,test-form))
     (declare (ignorable it))
     (when it
       ,@body)))

(defmacro awhile (test-form &body body)
  `(do ((it ,test-form ,test-form))
       ((not it))
     ,@body))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro while-bind ((var expr) &body body)
  `(do ((,var ,expr ,expr))
       ((not ,var))
     ,@body))

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if: use `it' in then-form, else-form to
   refer to result of the test-form."   ; LMH
  `(let ((it ,test-form))
     (declare (ignorable it))		; LMH
     (if it ,then-form ,else-form)))

(defmacro acond2 (&rest clauses)
  "Anaphoric cond for two returned values: use `it' in body to
   refer to result of the test.  Test can return two values,
   when is satisfied if either is true, but `it' refers to first."   ; LMH
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val))
		 (declare (ignorable it)); LMH
		 ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

; Iteration

(defmacro do-all (var x &rest body)
  (with-gensyms (g)
    `(let ((,g ,x))
       (if (consp ,g)
           (dolist (,var ,g) ,@body)
           (let ((,var ,g)) ,@body)))))

(defmacro dolists (pairs &rest body)
  (with-gensyms (f)
    (let ((parms (mapcar #'(lambda (p) (declare (ignore p)) (gensym)) pairs)))
      `(labels ((,f ,parms
                  (when (or ,@parms)
                    (let ,(mapcar #'(lambda (p g)
                                      (list (car p) `(car ,g)))
                                  pairs
                                  parms)
                      ,@body)
                    (,f ,@(mapcar #'(lambda (g) `(cdr ,g))
                                  parms)))))
         (,f ,@(mapcar #'cadr pairs))))))

(defmacro do3 (v1 v2 v3 list &rest body)
  (with-gensyms (g h)
    `(let ((,g ,list))
       (do ((,h ,g (cdr ,h)))
           ((null ,h) nil)
         (let ((,v1 (car ,h))
               (,v2 (if (cdr ,h) (cadr ,h) (car ,g)))
               (,v3 (if (cdr ,h)
                        (if (cddr ,h)
                            (third ,h)
                            (car ,g))
                        (if (cdr ,g)
                            (second ,g)
                            (car ,g)))))
           ,@body)))))

; Assumes 3 args.  Inefficient.

(defmacro do-cyclic (parms source &rest body)
  (let ((s (gensym)))
    `(let ((,s ,source))
       (case (length ,s)
         (0 nil)
         (1 (let ((,(first parms) (first ,s))
                  (,(second parms) (first ,s))
                  (,(third parms) (first ,s)))
              ,@body))
         (2 (let ((,(first parms) (second ,s))
                  (,(second parms) (first ,s))
                  (,(third parms) (second ,s)))
              ,@body)
            (let ((,(first parms) (first ,s))
                  (,(second parms) (second ,s))
                  (,(third parms) (first ,s)))
              ,@body))
         (t (do-tuples/c ,parms (rotlist ,s)
              ,@body))))))

(defmacro do-plist (v1 v2 plist &rest body)
  (with-gensyms (rec rest pl)
    `(labels ((,rec (,v1 ,v2 ,rest)
                 ,@body
                 (when ,rest
                   (,rec (car ,rest) (cadr ,rest) (cddr ,rest)))))
       (let ((,pl ,plist))
         (when (consp ,pl)
           (,rec (car ,pl) (cadr ,pl) (cddr ,pl)))))))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro casefn (val &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,val))
       (cond ,@(mapcar #'(lambda (cl)
                           `(,(if (or (eql (car cl) t) 
				      (eql (car cl) 'identity))
                                  t
                                  `(funcall #',(car cl) ,g))
                             ,@(cdr cl)))
                       clauses)))))

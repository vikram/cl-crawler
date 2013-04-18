(in-package :com.crawler)

(defparameter *fail* nil)

(defparameter *no-bindings* '((t . t)))

(defun optional-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\&)))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
	(if (and (eq bindings *no-bindings*))
	    nil
	    bindings)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t *fail*))))

(defparameter *occurs-check* t)

(defun occurs-check (var x bindings)
  (cond ((eq var x) t)
	((and (variable-p x) (get-binding x bindings))
	 (occurs-check var (lookup x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))

(defun unify-variable (var x bindings)
  (cond ((get-binding var bindings)
	 (unify (lookup var bindings) x bindings))
	((and (variable-p x) (get-binding x bindings))
	 (unify var (lookup x bindings) bindings))
	((and *occurs-check* (occurs-check var x bindings))
	 *fail*)
	(t (extend-bindings var x bindings))))

(defun unify (x y &optional (bindings *no-bindings*))
  (cond ((eq bindings *fail*) *fail*)
	((eql x y) bindings)
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((and (consp x) (consp y)) 
	 (unify (rest x) (rest y)
		(unify (first x) (first y) bindings)))
	(t *fail*)))
  
(proclaim '(inline reuse-cons))
(defun reuse-cons (x y x-y)
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defun subst-bindings (bindings x)
  (cond ((eq bindings *fail*) *fail*)
	((eq bindings *no-bindings*) x)
	((and (variable-p x) (get-binding x bindings))
	 (subst-bindings bindings (lookup x bindings)))
	((atom x) x)
	(t (reuse-cons (subst-bindings bindings (car x))
		       (subst-bindings bindings (cdr x))
		       x))))

(defun unifier (x y)
  (subst-bindings (unify x y) x))

(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation) (first relation))

(defvar *db-predicates* nil)

(defun add-clause (clause)
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
	  (nconc (get-clauses pred) (list clause)))
    pred))

(defmacro defrule (&rest clause)
  `(add-clause ',clause))

(defmacro fact (relation)
  `(defrule ,relation))

(defmacro rule (tail if &rest head)
  (declare (ignore if))
  `(defrule ,tail ,@head))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if 
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree)
				found-so-far))))

(defun variables-in (exp)
  (unique-find-anywhere-if #'variable-p exp))

(defun rename-variables (x)
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
		  (variables-in x))
	  x))

(defun clear-predicate (predicate)
  (setf (get predicate 'clauses) nil))

(defun clear-db ()
  (mapc #'clear-predicate *db-predicates*))

(defun prove-all (goals bindings)
  (cond ((eq bindings *fail*) *fail*)
	((null goals) (list bindings))
	(t (mapcan #'(lambda (goal1-solution)
		       (prove-all (rest goals) goal1-solution))
		   (prove (first goals) bindings)))))

(defun prove (goal bindings)
  (mapcan #'(lambda (clause)
	      (let ((new-clause (rename-variables clause)))
		(prove-all (clause-body new-clause)
			   (unify goal (clause-head new-clause) bindings))))
	  (get-clauses (predicate goal))))

(defun show-prolog-vars (vars bindings)
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
	(format t "~&~a = ~a" var
		(subst-bindings bindings var))))
  (princ ";"))

(defun show-prolog-solutions (vars solutions)
  (if (null solutions)
      (format t "~&No.")
      (mapc #'(lambda (solution) (show-prolog-vars vars solution))
	    solutions))
  (values))

(defun top-level-prove (goals)
  (show-prolog-solutions
   (variables-in goals)
   (prove-all goals *no-bindings*)))

(defmacro prolog-query (&rest goals)
  `(top-level-prove ',goals))

(defmacro prolog-query-any (&rest goals)
  `(prove-all ',goals *no-bindings*))

(defun make-generated-by-exp (from to slot)
  `(defrule (generated-by ,from ,to ,slot)))

(defun make-generated-by (from to slots)
  (loop while slots
       collecting (make-generated-by-exp from to (pop slots))))

(defmacro generate (from to &rest slots)
  `(progn
     ,@(make-generated-by from to slots)))


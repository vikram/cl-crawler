(in-package :com.crawler)

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun map-two (function list combiner)
  (cond ((null list) nil)
	((null (cddr list)) (funcall function (first list) (second list)))
	(t (funcall combiner (funcall function (first list) (second list))
		    (map-two function (cddr list) combiner)))))

(defun mappend (fn &rest lsts)
  (declare (function fn))   ; LMH
  "Nondestructive form of mapcan."   ; LMH
  (apply #'append (apply #'mapcar fn lsts)))

(defun firstn (lst n)
  (if (or (null lst) (<= n 0))
      nil
      (cons (car lst)
            (firstn (cdr lst) (- n 1)))))

(defun butlastn (seq n)
  (subseq seq 0 (- (length seq) n)))

(defun prefix (pref str)
  (search pref str :end2 (min (length pref) (length str))))
    
(defun suffix (str suff)
  (search suff str :start2 (- (length str) (length suff))))

(defun insert-before (before after lst)
  (cond ((null lst) nil)
        ((eql (car lst) after)
         (cons before lst))
        (t (cons (car lst) (insert-before before after (cdr lst))))))

(defun insert-after (before after lst)
  (cond ((null lst) nil)
        ((eql (car lst) before)
         (cons before (cons after (cdr lst))))
        (t (cons (car lst) (insert-after before after (cdr lst))))))

(defun delete-after (before lst &optional (step 1))
  (cond ((null lst) nil)
        ((eql (car lst) before)
	 (nthcdr step lst))
        (t (cons (car lst) (delete-after before (cdr lst) step)))))

(defun delete-nth (n lst)
  (cond ((< n 0) (error "Bad arg to delete-nth"))
        ((= n 0) (cdr lst))
        (t (let ((rest (nthcdr (1- n) lst)))
             (pop (cdr rest))
             lst))))

(defun ninsert-nth (n obj lst)
  (if (< n 0)
      (error "Bad arg to ninsert-nth")
      (let ((rest (nthcdr n lst)))
        (push obj (cdr rest))
        lst)))

(defun insert-elt-after-n (n ins lst)
  (if (null lst)
      nil
      (if (= 0 n)
	  (cons ins lst)
	  (if (= 1 n)
	      (cons (car lst) (cons ins (cdr lst)))
	      (cons (car lst) (insert-elt-after-n (1- n) ins (cdr lst)))))))
  
(defun insert-elt-after (elt ins lst)
  (if (null lst)
      nil     
      (if (eql (car lst) elt)
          (cons (car lst) (cons ins (cdr lst)))
          (cons (car lst) (insert-elt-after elt ins (cdr lst))))))

(defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))

(defun map0-n (fn n)
  (declare (function fn) (fixnum n))   ; LMH
  "Apply the fn to the list of numbers 0...n."  ; LMH
  (mapa-b fn 0 n))

(defun map1-n (fn n)
;  (declare (function fn) (fixnum n))   ; LMH
  "Apply the fn to the list of numbers 1...n."  ; LMH
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
 ; (declare (function fn) (fixnum a b step))   ; LMH
  "Apply the fn to the list of numbers a...b, stepping with step."  ; LMH
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
  ;  (declare (fixnum i))   ; LMH
    (push (funcall fn i) result)))

(defun map-subsequent-two (function list combiner)
  (declare (function function combiner) (list list))
  (cond ((null list) nil)
	((null (cddr list)) (funcall combiner (funcall function (first list) (second list)) nil))
	(t (funcall combiner (funcall function (first list) (second list))
		    (map-subsequent-two function (cdr list) combiner)))))

;(map-adj #'(lambda (x y) (+ x y))
;	 #'first
;	 #'second
;	 #'rest
;	 '(1 2 3 4 5 6 7 8 9 10))
;(3 5 7 9 11 13 15 17 19)

(defun map-adj (fn this adj rest list &optional (acc '()))
  (cond ((null list) (nreverse acc))
	((null (funcall rest (funcall rest list)))
	 (nreverse (cons (funcall fn (funcall this list) (funcall adj list))
			 acc)))
	(t (map-adj fn this adj rest (funcall rest list) 
		    (cons (funcall fn (funcall this list) (funcall adj list)) acc)))))

(defmacro defmap (name fn)
  (with-gensyms (lst)
    `(defun-exported ,name (,lst)
       (mapcar #',fn ,lst))))

(defun-exported clast (lst)
  (car (last lst)))

(defun upcase (str)
  (if (stringp str)
      (string-upcase str)
      str))

(defun downcase (str)
  (if (stringp str)
      (string-downcase str)
      str))

(defmap mcar car)
(defmap mclast clast)
(defmap mcdr cdr)
(defmap mbutlast butlast)
(defmap mup upcase)
(defmap mdown downcase)

(defun mid (lst)
  (let ((i 0))
    (mapcar #'(lambda (item) 
		(cons (incf i) item))
	    lst)))

(defun vec->lst (vec)
  (map 'list #'identity vec))

(defun lst->vec (lst)
  (map 'vector #'identity lst))

(defmacro defremove (name fn)
  (with-gensyms (lst)
    `(defun-exported ,name (,lst)
       (remove-if #',fn ,lst))))

(defremove remove-empty not)
(defremove remove-symbols symbolp)
(defremove remove-empty-string string-empty)

(defun-exported rest-if (list item &key (test #'eql) (key #'identity))
  (remove-if #'(lambda (x) (funcall test item (funcall key x))) list))

(in-package :com.crawler)

(defstruct trie (value nil) (arcs nil))
(defparameter *trie-deleted* 'deleted)

(defmethod print-object ((trie trie) stream)
  (when (trie-value trie)
    (print (trie-value trie) stream))
  (dolist (arc (trie-arcs trie))
    (format stream "~A->" (car arc))
    (print-object (cdr arc) stream)))

(defun follow-arc (component extend? trie)
  (let ((arc (assoc component (trie-arcs trie) :test #'string=)))
    (cond ((not (null arc)) (cdr arc))
	  ((not extend?) nil)
	  (t (let ((new-trie (make-trie)))
	       (push (cons component new-trie)
		     (trie-arcs trie))
	       new-trie)))))

(defun find-trie (key extend? trie)
  (cond ((null trie) nil)
	((atom key)
	 (if key
	     (follow-arc key extend? trie)
	     trie))
	(t (find-trie
	    (cdr key) extend?
	    (find-trie
	     (car key) extend?
	     (find-trie 
	      "." extend? trie))))))

(defun put-trie (key trie value)
  (let ((found (find-trie key t trie)))
    (if (trie-value found)
	(setf (trie-value found) (cons value (trie-value found)))
	(setf (trie-value found) (list value)))))

(defun get-trie (key trie)
  (let* ((key-trie (find-trie key nil trie))
	 (val (if key-trie (trie-value key-trie))))
    (if (or (null key-trie) (eql val *trie-deleted*))
	(values nil nil)
	(values val t))))

(defun delete-trie (key trie)
  (put-trie key trie *trie-deleted*))

(defun recurse-trie-for-value (trie &optional (acc '()))
  (if (not trie)
      acc
      (if (atom trie)
	  (if (trie-value trie)
	      (recurse-trie-for-value (trie-arcs trie) (append (trie-value trie) acc))
	      (recurse-trie-for-value (trie-arcs trie) acc))
	  (recurse-trie-for-value (cdr trie) 
				  (recurse-trie-for-value (cdr (car trie)) acc)))))

(defun suffix-array (string
		     &key 
		     (tokenizer #'letter-tokenizer)
		     (comparer #'(lambda (x y) (string<= (join x "") (join y "")))))
  (let* ((tokens (lst->vec (funcall tokenizer string)))
	 (len (length tokens)))
    (map 'vector #'car
	 (sort
	  (loop
	     :for i :from 0 :below len
	     :collect (cons i (make-array (- len i)
					  :element-type (array-element-type tokens)
					  :displaced-to tokens
					  :displaced-index-offset i)))
	  comparer
	  :key #'cdr))))

(defun generalized-suffix-tree (strings)
  (let ((trie (make-trie))
	(i 0))
    (dolist (str strings)
      (put-trie (map 'list #'identity str) trie (incf i)))
    trie))


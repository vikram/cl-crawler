(in-package :com.crawler)
  
(defun hash-table->alist (ht)
  "Return the alist with the same data as the hash-table.
Actually, the first element is the test: '(eql (key0 . val0) (key1 . val1)).
The inverse is `alist->hash-table'."
  (declare (hash-table ht))
  (cons (hash-table-test ht)
        (with-collect (co)
          (with-hash-table-iterator (iter ht)
            (loop (multiple-value-bind (re kk vv) (iter)
                    (unless re (return))
                    (co (cons kk vv))))))))

(defun alist->hash-table (alist &optional (value-fn #'identity))
  "Return the new hash-table based on this alist.
The inverse is `hash-table->alist'."
  (declare (list alist))
  (let ((ht (make-hash-table :test (car alist))))
    (dolist (co (cdr alist) ht)
      (setf (gethash (car co) ht) (funcall value-fn (cdr co))))))

(defmethod print-object ((ht hash-table) (out stream))
  (if *print-readably*
      (format out "~s" (hash-table->alist ht))
      (call-next-method)))

; Hash Tables

(defun nonempty-ht (ht)
  (maphash #'(lambda (k v) (return-from nonempty-ht t))
           ht)
  nil)

(defun ht-car (ht)
  (maphash #'(lambda (k v) (return-from ht-car v))
           ht))

(defun hash-keys (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v) 
                 (declare (ignore v)) 
                 (push k acc))
             ht)
    acc))

(defun hash-vals (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (push v acc))
             ht)
    acc))

(defun hash-pairs (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v)
                 (push (cons k v) acc))
             ht)
    acc))

(defun somehash (fn ht)
  (maphash #'(lambda (k v)
               (when (funcall fn v)
                 (return-from somehash v)))
           ht)
  nil)
     
(defun key-match (ht1 ht2)
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (when (gethash k ht2)
                 (return-from key-match k)))
           ht1)
  nil)

(defun write-hashtable-stream (stream ht)
  (when ht
    (maphash #'(lambda (key value)
		 (print (cons key value) stream)) 
	     ht)))

(defun write-hashtable (file ht)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (write-hashtable-stream stream ht)))

(defun read-hashtable-stream (stream)
  (let* ((ht (make-hash-table :test #'equal)))
    (loop for line = (read stream nil nil)
       until (null line)
       do
	 (setf (gethash (car line) ht) (cdr line)))
    ht))

(defun read-hashtable (file)
  (with-open-file (stream file :direction :input)
    (read-hashtable-stream stream)))

(defmacro-exported hash (&key (test nil) (rehash-size 10) (keyvals nil) (hash nil))
  ;--------------------------
  ; Return a new hash table.
  ;--------------------------
  `(progn
     (let ((h (make-hash-table ,@(when test (list :test test)) ,@(list :rehash-size rehash-size))))
       (when ,keyvals
	 (hash-populate h ,keyvals))
       (when ,hash
	 (do-hash (k v ,hash) (hash-put h k v)))
       h)))

(defmacro-exported hash-populate (hash kvlist)
  ;-----------------------------
  ; Insert KEY, VALUE elements 
  ;      from 2-element KVLIST
  ; Return Hash
  ; ----------------------------
  `(progn
     (dolist (kv ,kvlist)
       (hash-put ,hash (first kv) (second kv)))
     ,hash))

(defmacro-exported hash-put (hash key value)
  ;-----------------------------
  ; Insert KEY-VALUE into HASH.
  ; Return HASH.
  ;-----------------------------
  `(progn
     (setf (gethash ,key ,hash) ,value)
     ,hash))

(defmacro-exported hash-get (hash key)
  ;-----------------------------
  ; Get KEY-VALUE into HASH.
  ; Return HASH.
  ;-----------------------------
  `(progn
     (gethash ,key ,hash)
     ))

(defmacro-exported do-hash ((k v hash) &body body)
  ;-------------------------------------------------------
  ; Iterate over elements of HASH.  On each iteration
  ; evaluate BODY with ELEM bound to the current element.
  ;-------------------------------------------------------
  `(maphash
    (lambda (,k ,v)
      ,@body)
    ,hash))

;; Returns a new hashset that is a union of the two
(defmacro-exported hashset-union (hset1 hset2)
  `(let ((h (hash :test (hash-table-test ,hset1) :hash ,hset1)))
     (maphash #'(lambda (k v)
		  (hash-put h k v))
	      ,hset2)
     h))

;; Returns a new hashset that is a union of the two (destructive version)
(defmacro-exported nhashset-union (hset1 hset2)
  `(progn
    (maphash #'(lambda (k v)
		 (hash-put ,hset1 k v))
	     ,hset2)
     ,hset1))

;; Returns a new hashset that is the intersection
(defmacro-exported hashset-intersection (hset1 hset2)
  `(let* ((h (hash :test (hash-table-test ,hset1)))
	  (h1 (if (> (hash-table-size ,hset1) (hash-table-size ,hset2)) ,hset1 ,hset2))
	  (h2 (if (eq ,hset1 h1) ,hset2 ,hset1)))
     (maphash #'(lambda (k v)
		  (if (hash-get h1 k)
		      (hash-put h k v)))
	      h2)
     h))
  
(defmacro-exported hashset-difference (hset1 hset2)
  `(let ((h (hash :test (hash-table-test ,hset1))))
     (maphash #'(lambda (k v)
		  (unless (hash-get ,hset2 k) 
		    (hash-put h k v)))
	      ,hset1)
     h))

(defun-exported hash-empty? (hash)
  ;-------------------------
  ; True iff HASH is empty.
  ;-------------------------
  (null (hash-keys hash)))


(defun make-counter ()
  (hash :test 'equal))

(defun incf-counter (h key)
  (if (gethash key h)
      (incf (gethash key h))
      (setf (gethash key h) 1)))

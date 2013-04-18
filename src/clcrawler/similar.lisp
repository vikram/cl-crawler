(in-package :com.crawler)

(defun tokens->features (tokens)
  (let ((ht (make-counter)))
    (dolist (token tokens)
      (incf-counter ht token))
    ht))

(defun features->tf (features)
  (let* ((lst (cdr (hash-table->alist features)))
	 (len (length lst)))
    (dolist (feature lst)
      (setf (gethash (car feature) features)
	    (float (/ (cdr feature) len)))))
  features)

(defun tokens->tf (tokens)
  (features->tf (tokens->features tokens)))

(defun doc-tokens->df (docs-hts)
  (let ((ht (make-counter))
	(N (length docs-hts)))
    (dolist (doc-ht docs-hts)
      (do-hash (k v (cdr doc-ht))
	(incf-counter ht k)))
    (do-hash (k v ht)
      (setf (gethash k ht) 
	    (log (/ N v))))
    ht))

(defun make-hit-list (str)
  (tokens->tf (mapcar (compose #'string-downcase #'car) (word-tokenizer str))))

(defun cosine (vec1 vec2)
  (let ((den (sqrt (* (dot-product vec1 vec1) (dot-product vec2 vec2)))))
    (aif (zerop den)
	 1.0
	 (/ (dot-product vec1 vec2)
	    den))))

(defun dot-product (vec1 vec2)
  (let ((sum 0)
	(i 0))
    (maphash #'(lambda (k v)
		 (incf i)
		 (awhen (gethash k vec2)
		   (incf sum (* v it))))
	     vec1)
    (if (zerop i)
	1.0
	sum)))

(defun-exported string-similarity (str1 str2)
  (cosine
   (make-hit-list str1)
   (make-hit-list str2)))

(defun jaccard-coefficient (doc1 doc2)
  (float (/ (length (intersection doc1 doc2 :test #'string=))
	    (length (union doc1 doc2 :test #'string=)))))

(defun find-similar-documents (threshold)
  (let ((docs '()) (results '()) (any-similar nil))
    (with-read-line (line "/home/vb/repository/portfolio/crawler/data/rightmove-crawl/analysis/details")
      (push (cons line (shingles line :n 5)) docs))
    (dotimes (i (1- (length docs)))
      (let ((doc1 (nth i docs)))
	(setf any-similar nil)
	(dotimes (j (- (length docs) 2))
	  (let* ((doc2 (nth (+ i j 1) docs))
		 (similarity (jaccard-coefficient (cdr doc1) (cdr doc2))))
	    (when (and (not any-similar) (>= similarity threshold))
	      (print (list 'similar (car doc1) (car doc2) similarity))
	      (setf any-similar t))))
	(unless any-similar
	  (push (car doc1) results))))
    (with-open-file (out "/home/vb/repository/portfolio/crawler/data/rightmove-crawl/analysis/unique"
			 :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write results :stream out))))

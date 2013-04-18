(in-package :com.crawler)

(defun shingles (doc &key (n 3) (hash t))
  (let ((results '())
	(words (indexable-words doc)))
    (dotimes (i (1+ (- (length words) n)))
      (push (loop for j from 0 to (1- n)
	       collect (nth (+ i j) words))
	    results))
    (mapcar #'(lambda (shingle)
		(if hash 
		    (make-hash-code (format nil "~{~:@(~a~)~^ ~}" shingle))
		    (format nil "~{~:@(~a~)~^ ~}" shingle)))
	    (nreverse results))))

(defun ngrams-alphanumeric (str n)
  (cl-ppcre:all-matches-as-strings 
   (cl-ppcre:create-scanner `(:register 
			      (:sequence 
			       (:greedy-repetition ,n ,n (:CHAR-CLASS (:RANGE #\a #\z) (:RANGE #\A #\Z) (:RANGE #\0 #\9))))))
   str))

(defun ngram->vec (ngram)
  (let ((ht (make-counter)))
    (dolist (n ngram)
      (incf-counter ht n))
    ht))

(defun ngrams (str n)
  (let ((res '())
	(l (length str)))
    (dotimes (i l)
      (when (<= (+ i n) l)
	(push (trim-funny (subseq str i (+ i n))) res)))
    (nreverse res)))

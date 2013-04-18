
(defparameter *movies*
  (let ((mem '())
	(docid 0))
    (with-read-line (line "/home/vb/repository/puzzles/movies3.txt")
      (setf line (clean-line line))
      (push (cons (incf docid) line) mem))
    mem))

(defun movieid->name (id)
  (assoc id *MOVIES* :test #'=))

(defun clean-line (line)
  (if (char= #\Return (char line (1- (length line))))
      (subseq line 0 (1- (length line)))
      line))

(defparameter *movies-trie*
  (let ((trie (make-trie))
	(docid 0))
    (with-read-line (line "/home/vb/repository/puzzles/movies3.txt")
      (incf docid)
      (let* ((line (clean-line line))
	     (words (word-punc-splitter line))
	     (l (length words)))
	(dotimes (i l)
	  (let ((str (string-downcase (join (subseq words i) "% ")))) ; word-seperator
	    (if (= i 0)
		(put-trie (map 'list #'identity (s+ "^" str "$")) trie docid) ; start of name
		(put-trie (map 'list #'identity (s+ str "$")) trie docid)) ; rest of name
	    (put-trie (map 'list #'identity str) trie docid)))))
    trie))

(defun movie-link (word)
  (mapcar #'movieid->name
	  (recurse-trie-for-value 
	   (find-trie (map 'list #'identity (string-downcase word)) nil *movies-trie*))))

(defun followers (key)
  (mapcar #'(lambda (x) (movieid->name (car x)))
	  (remove-if-not 
	   #'(lambda (x) (eql :first (cdr x)))
	   (recurse-trie-for-value 
	    (find-trie (map 'list #'identity (string-downcase key)) nil *movies-trie*)))))

(defun leaders (key)
  (mapcar #'(lambda (x) (movieid->name (car x)))
	  (remove-if-not 
	   #'(lambda (x) (eql :last (cdr x)))
	   (recurse-trie-for-value 
	    (find-trie (map 'list #'identity (string-downcase key)) nil *movies-trie*)))))

(defparameter *movies-trie*
  (let ((trie (make-trie))
	(docid 0))
    (with-read-line (line "/home/vb/repository/puzzles/movies3.txt")
      (incf docid)
      (let* ((line (clean-line line))
	     (words (word-punc-splitter line))
	     (l (length words)))
	(dotimes (i l)
	  (let ((str (string-downcase (s+ (join (subseq words i) "% ") "%")))
		(rev (string-downcase (s+ (join (nreverse (subseq words i)) "% ") "%"))))
	    (put-trie (map 'list #'identity str) trie docid)
	    (put-trie (map 'list #'identity rev) trie docid)))
	(put-trie (map 'list #'identity (s+ "^" (string-downcase (join words "% ")) "%$")) trie docid)
	(put-trie (map 'list #'identity (s+ "$" (string-downcase (join (nreverse words) "% ")) "%^")) trie docid)))
    trie))

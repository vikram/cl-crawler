(in-package :com.crawler)

(ensure-directories-exist "/Users/vb/repository/portfolio/crawler/data/checkpoints/")
  
(defun checkpoint (filename &optional (pos nil))
  (if pos
      (with-open-file (out (format nil "/Users/vb/repository/portfolio/crawler/data/checkpoints/~A" (make-hash-code filename))
			   :direction :output :if-exists :supersede :if-does-not-exist :create)
	(write pos :stream out))
      (if (probe-file (format nil "/Users/vb/repository/portfolio/crawler/data/checkpoints/~A" (make-hash-code filename)))
	  (with-open-file (in (format nil "/Users/vb/repository/portfolio/crawler/data/checkpoints/~A" (make-hash-code filename)))
	    (read in))
	  0)))
 
(defun checkpoint-file-reader (filename
				 &key 
			       (delimiter #\linefeed) (buffer-size 512000) 
			       (restart-p nil) (abort-after nil))
  (let ((seq (make-array buffer-size :element-type 'character
			 :adjustable t
			 :fill-pointer buffer-size))
	(last-line nil)
	(lines '())
	(i 0))
    (when restart-p
      (checkpoint filename 0))
    #'(lambda ()
	(if (null lines)
	    (with-open-file (in filename)
	      (print 'cp-reading)
	      (file-position in (checkpoint filename))
	      (setf (fill-pointer seq) (read-sequence seq in)) 
	      (if (or (zerop (fill-pointer seq)))
		  (progn
		    (checkpoint filename (file-length in))
		    "")
		  (progn       
		    (setf lines (cl-ppcre:split (cl-ppcre:create-scanner '(:char-class #\return #\newline #\linefeed)) seq))
		    (awhen (first lines)
		      (setf last-line (car (last lines)))
		      (setf lines (cdr (butlast lines)))
		      (checkpoint filename (- (+ (checkpoint filename) (* (1+ i) buffer-size)) (length last-line)))
		      (print (cons 'read it))
		      (if (and abort-after (= (incf i) abort-after))
			  ""
			  it)))))
	      (awhen (first lines)
		(setf lines (rest lines))
		it)))))

(defun test-new-reader ()
(let ((doc "") (docid 0) (reader (checkpoint-file-reader "/home/vb/repository/portfolio/crawler/data/addresses.1000" :restart-p t)))
  (loop
     (setf doc (funcall reader))
     (when (string= doc "")
       (return))
     (progn
       (incf docid)
       (print (cons docid doc))))))

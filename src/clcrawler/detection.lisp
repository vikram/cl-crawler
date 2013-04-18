(in-package :com.crawler)

(defun page-signature (page)
  (flatten
   (mapcar #'(lambda (x) (if (stringp x) 
			     (make-word-list x) 
			     x))
	   (flatten page))))

(defun site-labeller (url labels)
  (let ((classifiers (make-hash-table)))
    (dolist (label labels)
      (setf (gethash (car label) classifiers)
	    (make-classifier :name (car label) 
			     :domain url 
			     :filter (cdr label) 
			     :classes '(true false) 
			     :hash (make-hash-table :test 'equal)))
    (read-all-pages 
     url 
     #'(lambda (url docid)
	 (let ((page (fetch-while-empty url docid 2)))
	   (when page
	     (dolist (label labels)
	       (let ((classifier (gethash (car label) classifiers)))
		 (train 
		  (page-signature page)
		  (classifier-hash classifier)
		  (if (cl-ppcre:all-matches (cdr label) url) 'in 'out)
		  classifier))))))))
    (save-site-labeller url labels classifiers)))
		   
(defun save-site-labeller (url labels classifiers)
  (let* ((domain (domain url))
	 (path (format nil "/home/vb/repository/portfolio/crawler/data/crawldb/~A/" domain)))
    (ensure-directories-exist path)
    (dolist (label labels)
      (with-open-file (out 
		       (format 
			nil 
			"/home/vb/repository/portfolio/crawler/data/crawldb/~A/~A.classifier" 
			domain 
			(car label))
		       :direction :output 
		       :if-exists :supersede 
		       :if-does-not-exist :create)
	(write (gethash (car label) classifiers) :stream out)))))

(defun getblocks (html &optional (elements *maybe-block-elements*))
  (let ((acc '()))
    (with-tags (elm html)
      (when (and (tag-p elm)
		 (tag-belongs-p elm elements))
	(push elm acc)))
    acc))

(defun process-web-class (main-url class-filter)
  (let ((pages '()))
    (read-all-pages 
     main-url 
     #'(lambda (url docid)
	 (push (cons url (getblocks (read-page url))) pages))
     :filter class-filter)))

(defstruct tagblock tag tfmatrix path tokens)

(defun get-tokenized-blocks (pages)
  (let ((page-blocks (hash :test 'equal)))
    (dolist (page pages)
      (hash-put page-blocks (car page) (hash))
      (let ((i 0)
	    (blocks (hash-get page-blocks (car page))))
	(dolist (blck (getblocks (prune-fluff (cdr page))))
	  (let ((tokens (tokenize-html blck)))
	    (hash-put blocks (incf i)
		      (make-tagblock :tag blck
				     :tfmatrix (tokens->tf tokens)
				     :path (tag-path blck)
				     :tokens tokens))))))
    page-blocks))

(defun similar-blocks (page-blocks)
  (let ((similar (hash :test 'equal))
	(pn-lst (hash-keys page-blocks)))
    (do-hash (pn tbs page-blocks)
      (do-hash (num tb tbs)
	(hash-put similar (cons pn num) '())
	(dolist (opn (remove pn pn-lst :test #'string=))
	  (let ((acc '()))
	    (do-hash (onum otb (hash-get page-blocks opn))
	      (when (reasonable-path-match 
		     (tagblock-path tb) 
		     (tagblock-path otb)) ;;assuming same path
		(simple-bind (sim (tagblock-similarity tb otb))
		  (when (and (> sim 0.9) 
			     (or (/= 1 (length (tagblock-tokens tb)))
				 (= (length (tagblock-tokens tb)) 
				    (length (tagblock-tokens otb)))))
		    (push (list onum 
				(tagblock-similarity tb otb) 
				(length (tagblock-tokens otb)) 
				(tagblock-path otb)) acc)))))
	    (push (list opn (sort acc #'> :key #'second)) 
		  (gethash (cons pn num) similar))))
	(setf (gethash (cons pn num) similar)
	      (list
	       (tagblock-path tb)
	       (length (tagblock-tokens tb)) 
	       (nreverse (gethash (cons pn num) similar))))))
    similar))
	 
(defun tagblock-similar-p (tb1 tb2 &optional (threshold 0.9))
  (> (tagblock-similarity tb1 tb2) threshold))

(defun tagblock-similarity (tb1 tb2)
  (cosine 
   (tagblock-tfmatrix tb1)
   (tagblock-tfmatrix tb2)))

(defun content-blocks (pages)
  (let* ((blocks (get-tokenized-blocks pages))
	 (sim-blocks (similar-blocks blocks)))
    (dolist (page (mcar pages))
      (dotimes (i 100)
	(print (list page (1+ i)
		     (reduce #'+
			     (gethash `(,page . ,(1+ i)) sim-blocks)
			     :key #'(lambda (bl) 
				      (aif (second (first (second bl)))
					   it
					   0))
			     :initial-value 0)))))))

(defun collect-paths (pages &key (prune t))
  (let ((acc '()))
    (dolist (page pages)
      (let ((page-acc '()))
	(with-tags (tag (if prune (prune-fluff (cdr page)) (cdr page)))
	  (push (tag-path tag) page-acc))
	(push (cons (car page) (tokens->features page-acc)) acc)))
    (let ((df (doc-tokens->df acc)))
      (values acc
	      df))))

(defun split-paths (df)
  (partition-if
   #'(lambda (x) (> (cdr x) (log 2)))
   (sort
    (cdr (hash-table->alist df))
    #'>
    :key #'cdr)))

(defun structural-similarity-degree (t1 t2)
  "assuming we've already got a reasonable location match"
  (equal (tag-structure t1)
	 (tag-structure t2)))

(defun choose-page (pages)
  (car
   (first
    (sort (cdr (hash-table->alist pages))
	  #'>
	  :key (compose #'hash-table-count #'cdr)))))

(defun class-adder (class)
  #'(lambda (tag) (tag-add-class tag class)))

(defun blck-similarity (blck)
  (fourth blck))

(defun blck-oth-blck (blck)
  (second blck))

(defun blck-structural (blck)
  (third blck))

(defun blck-oth (blck)
  (first blck))

(defun filter-sims (blcks threshold fn)
  (remove-if #'(lambda (blck) (funcall fn (blck-similarity blck) threshold)) blcks))

(defun blck-isdifferent (blcks)
  (null (filter-sims blcks 0.0 #'=)))

(defun average-sims (blcks)
  (if blcks
      (/ (reduce #'+ blcks :key #'blck-similarity)
	 (length blcks))
      0))



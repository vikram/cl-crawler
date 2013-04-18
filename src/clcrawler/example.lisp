(in-package :com.crawler)

;;first we need to make sure that the example corresponds to the downloaded page
;;not the latest version of the page. So we remember the page-path which the example is linked to.

(defparameter *examples* (hash :test 'equal))
(defparameter *examples-html* (hash :test 'equal))

(defun load-examples ()
  (setf *examples* (read-hashtable "/home/vb/repository/portfolio/crawler/data/examples"))
  (do-hash (k v *examples*)
    (hash-put *examples-html* (first v) (tag-html (gzip-stream:gunzip->string (second v))))))

(defun add-example (example-name path)
  (progn
    (hash-put *examples* example-name path)
    (write-hashtable "/home/vb/repository/portfolio/crawler/data/examples" *examples*)
    (hash-put *examples-html* (first path) (tag-html (gzip-stream:gunzip->string (second path))))))

(defun remove-example (example-name)
  (progn
    (remhash example-name *examples*)
    (write-hashtable "/home/vb/repository/portfolio/crawler/data/examples" *examples*)))

(load-examples)

(defun init-example (name url)
  (multiple-value-bind (html path)
      (fetch-while-empty url (make-hash-code url) 5)
    (add-example name (list url path))
    (hash-put *examples-html* url (tag-html html))))

(defun example-page (name url)
  (aif (hash-get *examples* name)
       (when (not (string= (first it) url))
	 (init-example name url))
       (init-example name url))
  (hash-get *examples-html* url))

(defun fieldexample1 (field)
  (second field))

(defun fieldexample2 (field)
  (third field))

(defun extracter-from-example (example eg-html)
  `((,(make-logic-symbol (symbol-name (record-name example))) (html)	
      (list-record
       ,(record-name example)
       ,@(call-extracter example eg-html)))
    ,@(mapcan #'(lambda (field)
		  (if (simple-funcall-p field)
		      (list (second field))
		      (if (record-loop-p field)
			  (record-loop-funcall field))))
	      (record-fields example))))

(defun record-loop-funcall (field)
  (mapcan #'(lambda (f)
	      (if (record-loop-p f)
		  (record-loop-funcall f)
		  (list (second f))))
	  (record-fields field)))

(defun call-extracter (record eg-html)
  (let ((i 0))
    (mapcar #'(lambda (field)
		(incf i)
		(if (simple-funcall-p field)
		    `(list ,(first field) (,(first (second field)) html))
		    (if (record-loop-p field)
			(record-loop-extracter field 'html eg-html))))
          (record-fields record))))

(defun record-loop-extracter (record-loop h eg-html)
  "either it's straight forward and loop has many kids, etc.
   or we need to go child tag by child tag to see how far we are in the loop.
   kind of like a lookahead or a yield."
  (with-gensyms (rl-html field-no field-fns some-html)
    (if (> (length (all-ext-by-path eg-html (loop-path (record-fields record-loop)))) 1)
	`(mapcar #'(lambda (,rl-html)
		 (when *debug-extracter* (print (list ,(record-name record-loop) (page-text ,rl-html))))
		 (list-record
		  ,(record-name record-loop)
		  ,@(mapcar #'(lambda (f)
				(if (record-loop-p f)
				    (record-loop-extracter f rl-html
							   (first (all-ext-by-path eg-html (loop-path (record-fields record-loop)))))
				    `(list ,(first f) (,(first (second f)) ,rl-html))))
			    (record-fields record-loop))))
		 (all-ext-by-path ,h ',(loop-path (record-fields record-loop))))
	(progn
	  (when *debug-extracter* (print (list 'rle record-loop)))
	`(loop-collecting
	    :html ',(loop-path (record-fields record-loop))
	    :from ,h
	    :paths ',(mapcar #'(lambda (f) (multiple-value-list (field-path f))) (record-fields record-loop))
	    :record ,(record-name record-loop)
	    :fields #'(lambda (,rl-html ,field-no)
			(let ((,field-fns (list ,@(mapcar #'(lambda (f)
							      (if (record-loop-p f)
								  `#'(lambda (,some-html)
								       ,(record-loop-extracter 
									 f 
									 some-html
									 (first (all-ext-by-path eg-html (loop-path (record-fields record-loop))))))
								  `#'(lambda (,some-html) (list ,(first f) (,(first (second f)) ,some-html)))))
							  (record-fields record-loop)))))
			  (funcall (nth ,field-no ,field-fns) ,rl-html))))))))

(defun loop-collecting (&key html from paths record fields)
  "Why do I think that we don't need the detail functions anymore,
   as we have the paths anyway. But what about deeper nesting.
   I also think that fields is wrong. It shouldn't be taking entire 
   sections but probably be seperate functions in a list or a closure."
  (let ((init (first (ext-by-path from html :full nil)))
	(collection '())
	(collections '()))
    (print (page-text from))
    (when init
      (stp:do-children (child init)
	(let* ((child-path (tag-path child nil t))
	       (which-path (position child-path paths 
				     :key #'first 
				     :test #'(lambda (t1 t2) 
					       (let ((l (min (length t1) (length t2))))
						 (tag-path-equal (subseq t1 0 l)
								 (subseq t2 0 l)))))))
	  (when which-path
	    (when (zerop which-path)
	      (when collection
		(push (append (list-record record) (nreverse collection)) collections))
	      (setf collection '()))
	    (push (funcall fields child which-path)
		  collection))))
      (when collection
	(push (append (list-record record) (nreverse collection)) collections))
      (nreverse (remove-empty collections)))))

(defun field-path (field)
  (if (record-loop-p field)
      (values (loop-path (record-fields field)) :loop)
      (values (third field) :single)))

(defun loop-path (fields)
  (when *debug-extracter* (print (list 'lp fields)))
  (if (same (mapcar #'field-path fields) :test #'equal)
      (third (first fields))
      (common (third (first fields))
	      (field-path (second fields))
	      :test #'equal)))

(defun simple-funcall-p (field)
  (and (listp field)
       (symbolp (first field))
       (listp (second field))
       (symbolp (first (second field)))
       (listp (second (second field)))
       (eql (first (second (second field))) 'html)))

(defmacro defexample (n example url)
  (let ((name (make-keyword (keyword-name n))))
    (with-gensyms (page)
      `(defun ,(make-logic-symbol (s+ (symbol-name name) "-wrapper")) (,page)
	 (labels (,@(extracter-from-example 
		     (learn-from-example
		      example
		      (prune-fluff (example-page name url)))
		     (prune-fluff (example-page name url))))
	   (,(make-logic-symbol (symbol-name (record-name example))) ,page))))))

(defun extracter-builder (record)
  "creates the actual extracter function"
  (let ((i 0))
    (mapcar #'(lambda (field)
		(incf i)
		(if (simple-funcall-p field)
		    `(list ,(first field)
			   (,(make-logic-symbol (s+ (symbol-name (record-name record))
						    "-"
						    (symbol-name (first field))))
			     'html))))
	    (record-fields record))))

(defun learn-from-example (example eg-html)
  (build-extracter
   (find-paths-from-example example eg-html)
   eg-html))

(defun elmtext2 (q page templates visited)
  (if (simple-field-p q)
      (list (first q) (elmtext3 (fieldexample1 q) page templates))
      (if (record-loop-field-p q)
	  (progn
	    (when *debug-extracter* (print (list :fieldexample1 (elmtext3 (fieldexample1 q) page templates))))
	    (list (first q) (elmtext3 (fieldexample1 q) page templates)
		  (elmtext3 (fieldexample2 q) page templates)))
	  (list :wierd q))))

(defun elmtext3 (string page templates)
  (let ((scanner (cl-ppcre:create-scanner string)))
    (with-tags (text page)
      (when (eql (type-of text) 'stp:text)
	(let ((data (trim (stp:data text))))
	  (awhen (cl-ppcre:all-matches scanner (trim (stp:data text)))
	    (when-bind (match (find (tag-path text) templates :test #'equal :key #'car))
	      (when-bind (real-match (cl-ppcre:all-matches scanner (second match)))
		(setf it real-match)
		(setf data (second match))))
	    (return (list (tag-path text nil t)
			  (template-pattern it data)))))))))

(defun make-elmtext2 ()
  (let ((templates '())
        (visited '()))
    #'(lambda (q page record)
        (awhen (elmtext2 q page templates visited)
          (unless (equal (first visited) (first it))
            (push (first it) visited))
          (push it templates)
	  (when *debug-extracter* 
	    (print (list :me2 q))
	    (print it))
	  it))))

(defun example-parser (example page &key (extracter (make-elmtext2)))
  (if (not example)
	nil
	(if (record-p example)
	    (eg-parse-record example page extracter)
	    (if (record-loop-p example)
		(eg-parse-record-loop example page extracter)
		(progn
		  (when *debug-extracter* (print (list :error example)))
		  (cons :error example)))))) 

(defun eg-parse-record-loop (example page extracter)
  (append (list :record-loop (record-name example))
	  (mapcar #'(lambda (field)
		      (when *debug-extracter* (list :rl field))
		      (if (or (record-loop-field-p field) (example-record-loop-field-p field))
			  (funcall extracter field page example)
			  (example-parser field page :extracter extracter)))
		  (record-fields example))))

(defun record-loop-p (example)
  (eql (first example) :record-loop))
 
(defun record-p (record)
  (eql (first record) :record))

(defun eg-parse-record (example page extracter)
  (append (list :record (record-name example))
	  (mapcar #'(lambda (field)
		      (if (or (simple-field-p field) (example-field-p field))
			  (funcall extracter field page example)
			  (example-parser field page :extracter extracter)))
		  (record-fields example))))

(defun simple-field-p (field)
  (and (listp field)
       (simple-listp field)
       (symbolp (first field))
       (stringp (second field))
       (not (third field))))

(defun record-loop-field-p (field)
  (and (listp field)
       (simple-listp field)
       (symbolp (first field))
       (stringp (second field))
       (stringp (third field))))

(defun example-field-p (field)
  (and (listp field)
       (symbolp (first field))
       (listp (second field))
       (listp (first (second field)))
       (stringp (second (second field)))
       (not (third field))))

(defun example-record-loop-field-p (field)
  (and (listp field)
       (symbolp (first field))
       (listp (second field))
       (listp (third field))
       (stringp (second (second field)))
       (stringp (second (third field)))))
	   
(defun simple-loop-field-p (field)
  (and (listp field)
       (simple-listp field)
       (symbolp (first field))
       (stringp (second field))
       (stringp (third field))))

(defun elmwrapper2 (q page)
  (ext-template (second (second q))
                (trim
                 (stp:data
                  (stp:nth-child
                   (parse-integer (fourth (multiple-value-list (loc->split (clast (first (second q)))))))
                   (first (ext-by-path page (butlast (first (second q))))))))))

(defun build-extracter (example page)
  (example-parser 
   example page 
   :extracter #'(lambda (q page record)
		  (if (or (simple-field-p q) (example-field-p q))
		      (field-extracter q record page)
		      (if (or (record-loop-field-p q) (example-record-loop-field-p q))
			  (field-loop-extracter q record page))))))

(defun find-paths-from-example (example page)
  (example-parser example page :extracter (make-elmtext2)))

(defun field-extracter(field record page)
  (multiple-value-bind (start end)
      (template-start-end (second (second field)))
    `(,(first field) 
       (,(make-logic-symbol (s+ (symbol-name (record-name record))
				"-"
				(symbol-name (first field))))
	(html)
	(template-text
	 ,(if (> (length (all-ext-by-path page (if (is-text-path (first (second field)))
									(butlast (first (second field)))
									(first (second field)))))
				    1)
	      `(ext-by-pos html ',(first (second field)))
              `(ext-by-path html ',(first (second field)) :full nil))
	 :start ,start
	 :end ,end)))))

(defun field-loop-extracter(field record page) 
  (let ((com (common (first (second field))
		     (first (third field)) 
		     :test #'equal)))
    (when *debug-extracter* (print (list 'fle record field)))
    (multiple-value-bind (start end)
	(template-start-end (second (second field)))
      `(,(first field) 
	 (,(make-logic-symbol (s+ (symbol-name (record-name record))
				  "-"
				  (symbol-name (first field))))
	  (html)
	  (template-text
	   ,(if (> (length 
		    (all-ext-by-path 
		     (first 
		      (all-ext-by-path 
		       page 
		       (subseq (first (second field)) 0 (1+ (length com)))))
		     (if (is-text-path (first (second field)))
			 (butlast (first (second field)))
			 (first (second field)))))
		   1)
	      `(ext-by-pos-loop html ',(first (second field)) ',(subseq (first (second field)) 0 (1+ (length com))))
              `(ext-by-path html ',(first (second field)) :full nil))
	   :start ,start
	   :end ,end))
	 ,(subseq (first (second field)) 0 (1+ (length com)))))))

(defun record-field-name (field-name record)
   (find field-name (record-fields record) :key #'first :test #'eql))

(defun record-fields-name (field-name record)
  (mapcar #'(lambda (x) (crawler::record-field-name field-name x))
	  record))

;;to debug the defexample macro try to first see if
;;learn-from-example works then check if extracter-from-example works
